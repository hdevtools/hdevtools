{-# LANGUAGE CPP, ViewPatterns #-}

module CommandLoop
    ( newCommandLoopState
    , Config(..)
    , CabalConfig(..)
    , updateConfig
    , startCommandLoop
    ) where

import Control.Exception
import Control.Applicative ((<|>))
import Control.Monad (when, void)
import Data.IORef
import Data.List (find, intercalate)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif
import MonadUtils (MonadIO, liftIO)
import System.Directory (setCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory)
import qualified ErrUtils
import qualified Exception (ExceptionMonad)
#if __GLASGOW_HASKELL__ >= 800
import qualified DynFlags
#endif
import qualified GHC
import qualified GhcPlugins as GHC
import qualified ErrUtils as GHC
import qualified GHC.Paths
import qualified Outputable
import System.Posix.Types (EpochTime)
import System.Posix.Files (getFileStatus, modificationTime)

import Types (ClientDirective(..), Command(..), CommandExtra(..))
import GhcTypes (needsTemplateHaskellOrQQ, getModSummaries)
import Info (getIdentifierInfo, getType)
import FindSymbol (findSymbol)
import Cabal (getPackageGhcOpts)
import Stack

type ClientSend = ClientDirective -> IO ()

data State = State
    { stateWarningsEnabled :: Bool
    }

newCommandLoopState :: IO (IORef State)
newCommandLoopState = 
    newIORef $ State
        { stateWarningsEnabled = True
        }

data CabalConfig = CabalConfig
    { cabalConfigPath :: FilePath
    , cabalConfigOpts :: [String]
    , cabalConfigLastUpdatedAt :: EpochTime
    }
    deriving (Eq, Show)

mkCabalConfig :: FilePath -> [String] -> IO CabalConfig
mkCabalConfig path opts = do
    fileStatus <- getFileStatus path
    return $ CabalConfig { cabalConfigPath = path
                         , cabalConfigOpts = opts
                         , cabalConfigLastUpdatedAt = modificationTime fileStatus
                         }

data Config = Config
    { configGhcOpts :: [String]
    , configCabal   :: Maybe CabalConfig
    , configStack   :: Maybe StackConfig
    , configTH      :: Bool
    }
    deriving (Eq, Show)

updateConfig :: Maybe Config -> CommandExtra -> IO Config
updateConfig mConfig cmdExtra = do
    mbCabalConfig <- traverse (\path -> mkCabalConfig path (ceCabalOptions cmdExtra)) $
      ceCabalFilePath cmdExtra

    mbStackConfig <- if (stackYaml <$> msc) == ceStackYamlPath cmdExtra
      then return msc
      else getStackConfig cmdExtra

    return $ Config { configGhcOpts = "-O0" : ceGhcOptions cmdExtra
                    , configCabal = mbCabalConfig
                    , configStack = mbStackConfig
                    , configTH    = ceTemplateHaskell cmdExtra
                    }
 where
  msc = mConfig >>= configStack

type CommandObj = (Command, Config)

withWarnings :: (MonadIO m, Exception.ExceptionMonad m) => IORef State -> Bool -> m a -> m a
withWarnings state warningsValue action = do
    beforeState <- liftIO getWarnings
    liftIO $ setWarnings warningsValue
    action `GHC.gfinally` liftIO (setWarnings beforeState)
    where
    getWarnings :: IO Bool
    getWarnings = stateWarningsEnabled <$> readIORef state
    setWarnings :: Bool -> IO ()
    setWarnings val = modifyIORef state $ \s -> s { stateWarningsEnabled = val }

startCommandLoop :: IORef State -> ClientSend -> IO (Maybe CommandObj) -> Config -> Maybe Command -> IO ()
startCommandLoop state clientSend getNextCommand initialConfig mbInitialCommand = do
    continue <- GHC.runGhc ghcLibDir $ do
        configResult <- configSession state clientSend initialConfig
        case configResult of
          Left e -> do
              liftIO $ mapM_ clientSend
                  [ ClientStderr e
                  , ClientExit (ExitFailure 1)
                  ]
              processNextCommand True
          Right _ -> do
              doMaybe mbInitialCommand $ \cmd -> sendErrors (runCommand state clientSend initialConfig cmd)
              processNextCommand False

    case continue of
        Nothing ->
            -- Exit
            return ()
        Just (cmd, config) -> startCommandLoop state clientSend getNextCommand config (Just cmd)
  where
    ghcLibDir = stackGhcLibDir <$> configStack initialConfig
                <|> Just GHC.Paths.libdir

    processNextCommand :: Bool -> GHC.Ghc (Maybe CommandObj)
    processNextCommand forceReconfig = do
        mbNextCmd <- liftIO getNextCommand
        case mbNextCmd of
            Nothing ->
                -- Exit
                return Nothing
            Just (cmd, config) ->
                if forceReconfig || (config /= initialConfig)
                    then return (Just (cmd, config))
                    else do
                        sendErrors (runCommand state clientSend initialConfig cmd)
                        processNextCommand False

    sendErrors :: GHC.Ghc () -> GHC.Ghc ()
    sendErrors action = 
            action `GHC.gcatch` ghcError
                   `GHC.gcatch` sourceError
                   `GHC.gcatch` unknownError
        where
            ghcError :: GHC.GhcException -> GHC.Ghc ()
            ghcError = die . flip GHC.showGhcException ""

            unknownError :: SomeException -> GHC.Ghc ()
            unknownError = die . show

            sourceError :: GHC.SourceError -> GHC.Ghc ()
            sourceError = report

            die msg = liftIO $ mapM_ clientSend
                [ ClientStderr msg
                , ClientExit (ExitFailure 1)
                ]

            report (GHC.srcErrorMessages -> bag) = do
                flags <- GHC.getSessionDynFlags
                let msgs = map (Outputable.showSDoc flags) $ GHC.pprErrMsgBagWithLoc bag
                liftIO $ do
                    mapM_ (logActionSend state clientSend GHC.SevError) msgs
                    clientSend $ ClientExit ExitSuccess


doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

configSession :: IORef State -> ClientSend -> Config -> GHC.Ghc (Either String ())
configSession state clientSend config = do
    eCabalGhcOpts <- case configCabal config of
                      Nothing ->
                          return $ Right []
                      Just cabalConfig -> do
                          liftIO $ setCurrentDirectory . takeDirectory $ cabalConfigPath cabalConfig
                          liftIO $ getPackageGhcOpts (cabalConfigPath cabalConfig) (configStack config) (cabalConfigOpts cabalConfig)
    case eCabalGhcOpts of
      Left e -> return $ Left e
      Right cabalGhcOpts -> do
          let allGhcOpts = cabalGhcOpts ++ configGhcOpts config
          GHC.gcatch (Right <$> updateDynFlags allGhcOpts)
                     (fmap Left . handleGhcError)
  where
    updateDynFlags :: [String] -> GHC.Ghc ()
    updateDynFlags ghcOpts = do
        initialDynFlags <- GHC.getSessionDynFlags
        let updatedDynFlags = initialDynFlags
                { GHC.log_action    = logAction state clientSend
                , GHC.ghcLink       = GHC.NoLink
                , GHC.hscTarget     = GHC.HscNothing
                }
        (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc ghcOpts)
        _ <- GHC.setSessionDynFlags finalDynFlags
        return ()

    handleGhcError :: GHC.GhcException -> GHC.Ghc String
    handleGhcError e = return $ GHC.showGhcException e ""

loadTarget :: [FilePath] -> Config -> GHC.Ghc (Maybe GHC.SuccessFlag)
loadTarget files conf = do
    let noPhase = Nothing
    targets <- mapM (`GHC.guessTarget` noPhase) files
    GHC.setTargets targets
    graph <- GHC.depanal [] True
    if configTH conf || not (needsTemplateHaskellOrQQ graph)
        then do
            when (needsTemplateHaskellOrQQ graph) $ do
                flags <- GHC.getSessionDynFlags
                void . GHC.setSessionDynFlags $ flags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink = GHC.LinkInMemory }
            Just <$> GHC.load GHC.LoadAllTargets
        else return Nothing

withTargets :: ClientSend -> [FilePath] -> Config -> GHC.Ghc () -> GHC.Ghc ()
withTargets clientSend files conf act = do
    ret <- loadTarget files conf
    case ret of
        Nothing -> liftIO $ mapM_ clientSend [ClientStderr "Template haskell required but not activated", ClientExit (ExitFailure 1)]

        Just GHC.Failed -> liftIO $ mapM_ clientSend [ClientStderr "Failed to load targets", ClientExit (ExitFailure 1)]

        Just GHC.Succeeded -> act

runCommand :: IORef State -> ClientSend -> Config -> Command -> GHC.Ghc ()
runCommand _ clientSend conf (CmdCheck file) =
    withTargets clientSend [file] conf
        (liftIO . clientSend . ClientExit $ ExitSuccess)
runCommand _ clientSend _ (CmdModuleFile moduleName) = do
    target <- GHC.guessTarget moduleName Nothing
    GHC.setTargets [target]
    res <- GHC.load GHC.LoadAllTargets
    case res of
      GHC.Failed -> liftIO $ mapM_ clientSend [ ClientStderr "Error loading targets"
                                              , ClientExit (ExitFailure 1)
                                              ]
      GHC.Succeeded -> do
        modSummaries <- getModSummaries
        case find (moduleSummaryMatchesModuleName moduleName) modSummaries of
          Nothing -> liftIO $ mapM_ clientSend [ ClientStderr "Module not found"
                                               , ClientExit (ExitFailure 1)
                                               ]
          Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
              Nothing -> liftIO $ mapM_ clientSend [ ClientStderr "Module does not have a source file"
                                                   , ClientExit (ExitFailure 1)
                                                   ]
              Just file -> liftIO $ mapM_ clientSend [ ClientStdout file
                                                     , ClientExit ExitSuccess
                                                     ]
  where
    moduleSummaryMatchesModuleName modName modSummary =
      modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand state clientSend conf (CmdInfo file identifier) =
    withTargets clientSend  [file] conf $ do
        result <- withWarnings state False $
            getIdentifierInfo file identifier
        case result of
            Left err ->
                liftIO $ mapM_ clientSend
                    [ ClientStderr err
                    , ClientExit (ExitFailure 1)
                    ]
            Right info -> liftIO $ mapM_ clientSend
                [ ClientStdout info
                , ClientExit ExitSuccess
                ]
runCommand state clientSend conf (CmdType file (line, col)) =
    withTargets clientSend [file] conf $ do
        result <- withWarnings state False $
            getType file (line, col)
        case result of
            Left err ->
                liftIO $ mapM_ clientSend
                    [ ClientStderr err
                    , ClientExit (ExitFailure 1)
                    ]
            Right types -> liftIO $ do
                mapM_ (clientSend . ClientStdout . formatType) types
                clientSend (ClientExit ExitSuccess)
        where
        formatType :: ((Int, Int, Int, Int), String) -> String
        formatType ((startLine, startCol, endLine, endCol), t) =
            concat
                [ show startLine , " "
                , show startCol , " "
                , show endLine , " "
                , show endCol , " "
                , "\"", t, "\""
                ]
runCommand state clientSend conf (CmdFindSymbol symbol files) = do
    -- for the findsymbol command GHC shouldn't output any warnings
    -- or errors to stdout for the loaded source files, we're only
    -- interested in the module graph of the loaded targets
    dynFlags <- GHC.getSessionDynFlags
    _        <- GHC.setSessionDynFlags dynFlags { GHC.log_action = \_ _ _ _ _ ->
#if __GLASGOW_HASKELL__ >= 800
                                                 return . return $ () }
#else
                                                 return () }
#endif

    ret <- withTargets clientSend files conf $ do
        result <- withWarnings state False $ findSymbol symbol
        case result of
            []      -> liftIO $ mapM_ clientSend
                        [ ClientStderr $ "Couldn't find modules containing '" ++ symbol ++ "'"
                        , ClientExit (ExitFailure 1)
                        ]
            modules -> liftIO $ mapM_ clientSend
                        [ ClientStdout (intercalate "\n" modules)
                        , ClientExit ExitSuccess
                        ]
    -- reset the old log_action
    _ <- GHC.setSessionDynFlags dynFlags
    return ret



#if __GLASGOW_HASKELL__ >= 800
logAction :: IORef State -> ClientSend -> GHC.DynFlags -> DynFlags.WarnReason -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction state clientSend dflags _ severity srcspan style msg =
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage severity srcspan msg
#elif __GLASGOW_HASKELL__ >= 706
logAction :: IORef State -> ClientSend -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction state clientSend dflags severity srcspan style msg =
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage severity srcspan msg
#else
logAction :: IORef State -> ClientSend -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction state clientSend severity srcspan style msg =
    let out = Outputable.renderWithStyle fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage srcspan msg
#endif

logActionSend :: IORef State -> ClientSend -> GHC.Severity -> String -> IO ()
logActionSend state clientSend severity out = do
    currentState <- readIORef state
    when (not (isWarning severity) || stateWarningsEnabled currentState) $
        clientSend (ClientStdout out)
    where
    isWarning :: GHC.Severity -> Bool
    isWarning GHC.SevWarning = True
    isWarning _ = False
