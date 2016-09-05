{-# LANGUAGE CPP #-}
module CommandLoop
    ( newCommandLoopState
    , Config(..)
    , CabalConfig(..)
    , updateConfig
    , startCommandLoop
    ) where

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
import qualified GHC.Paths
import qualified Outputable
import System.Posix.Types (EpochTime)
import System.Posix.Files (getFileStatus, modificationTime)

import Types (ClientDirective(..), Command(..), CommandExtra(..))
import Info (getIdentifierInfo, getType)
import FindSymbol (findSymbol)
import Cabal (getPackageGhcOpts)
import Stack

type ClientSend = ClientDirective -> IO ()

data State = State
    { stateWarningsEnabled :: Bool
    }

newCommandLoopState :: IO (IORef State)
newCommandLoopState = do
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

    mbStackConfig <- if (stackYaml <$> msc) == (ceStackYamlPath cmdExtra)
      then return msc
      else getStackConfig cmdExtra

    return $ Config { configGhcOpts = "-O0" : ceGhcOptions cmdExtra
                    , configCabal = mbCabalConfig
                    , configStack = mbStackConfig
                    , configTH    = True
                    }
 where
  msc = mConfig >>= configStack

type CommandObj = (Command, Config)

withWarnings :: (MonadIO m, Exception.ExceptionMonad m) => IORef State -> Bool -> m a -> m a
withWarnings state warningsValue action = do
    beforeState <- liftIO $ getWarnings
    liftIO $ setWarnings warningsValue
    action `GHC.gfinally`
        (liftIO $ setWarnings beforeState)
    where
    getWarnings :: IO Bool
    getWarnings = readIORef state >>= return . stateWarningsEnabled
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
              doMaybe mbInitialCommand $ \cmd -> sendErrors (runCommand state clientSend (configTH initialConfig) cmd)
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
                    else sendErrors (runCommand state clientSend (configTH initialConfig) cmd) >> processNextCommand False

    sendErrors :: GHC.Ghc () -> GHC.Ghc ()
    sendErrors action = GHC.gcatch action $ \e -> do
        liftIO $ mapM_ clientSend
            [ ClientStderr $ GHC.showGhcException e ""
            , ClientExit (ExitFailure 1)
            ]
        return ()

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

runCommand :: IORef State -> ClientSend -> Bool -> Command -> GHC.Ghc ()
runCommand _ clientSend th (CmdCheck file) = do
    let noPhase = Nothing
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]
    graph <- GHC.depanal [] True
    if th || (not $ GHC.needsTemplateHaskell graph)
        then do
           when (GHC.needsTemplateHaskell graph) $ do
               flags <- GHC.getSessionDynFlags
               void . GHC.setSessionDynFlags $ flags { GHC.hscTarget = GHC.HscInterpreted, GHC.ghcLink = GHC.LinkInMemory }
           let handler err = GHC.printException err >> return GHC.Failed
           flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
           liftIO $ case flag of
               GHC.Succeeded -> clientSend (ClientExit ExitSuccess)
               GHC.Failed -> clientSend (ClientExit (ExitFailure 1))
        else liftIO $ mapM_ clientSend [ ClientStderr "Template haskell required but not activated"
                                       , ClientExit (ExitFailure 1)]
runCommand _ clientSend _ (CmdModuleFile moduleName) = do
    moduleGraph <- GHC.getModuleGraph
    case find (moduleSummaryMatchesModuleName moduleName) moduleGraph of
        Nothing ->
            liftIO $ mapM_ clientSend
                [ ClientStderr "Module not found"
                , ClientExit (ExitFailure 1)
                ]
        Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
                Nothing ->
                    liftIO $ mapM_ clientSend
                        [ ClientStderr "Module does not have a source file"
                        , ClientExit (ExitFailure 1)
                        ]
                Just file ->
                    liftIO $ mapM_ clientSend
                        [ ClientStdout file
                        , ClientExit ExitSuccess
                        ]
    where
    moduleSummaryMatchesModuleName modName modSummary =
        modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand state clientSend _ (CmdInfo file identifier) = do
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
runCommand state clientSend _ (CmdType file (line, col)) = do
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
runCommand state clientSend _ (CmdFindSymbol symbol files) = do
    result <- withWarnings state False $ findSymbol symbol files
    case result of
        []      -> liftIO $ mapM_ clientSend
                       [ ClientStderr $ "Couldn't find modules containing '" ++ symbol ++ "'"
                       , ClientExit (ExitFailure 1)
                       ]
        modules -> liftIO $ mapM_ clientSend
                       [ ClientStdout (formatModules modules)
                       , ClientExit ExitSuccess
                       ]
    where
    formatModules = intercalate "\n"



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
