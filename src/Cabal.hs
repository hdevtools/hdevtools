{-# LANGUAGE CPP #-}
module Cabal
  ( getPackageGhcOpts
  , findCabalFile, findFile
  ) where

import Stack
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (execStateT, modify)
import Data.Char (isSpace)
import Data.List (foldl', nub, sort, find, isPrefixOf, isSuffixOf)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(..))
#endif
#if __GLASGOW_HASKELL__ < 802
import Distribution.Package (PackageIdentifier(..), PackageName)
#endif
import Distribution.PackageDescription (PackageDescription(..), Executable(..), TestSuite(..), Benchmark(..), emptyHookedBuildInfo, buildable, libBuildInfo)
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), Component(..), componentName, componentBuildInfo)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.Command (CommandParse(..), commandParseArgs)
import Distribution.Simple.GHC (componentGhcOptions)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.Types (ConfiguredProgram(programVersion), simpleProgram)
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Setup (ConfigFlags(..), defaultConfigFlags, configureCommand, toFlag, flagToMaybe)
#if MIN_VERSION_Cabal(1,21,1)
import Distribution.Utils.NubList
#endif
import Distribution.Verbosity (silent)
import Distribution.Version

import qualified Distribution.PackageDescription as Distribution
#if MIN_VERSION_Cabal(2, 2, 0)
import qualified Distribution.PackageDescription.Parsec as Distribution
#else
import qualified Distribution.PackageDescription.Parse as Distribution
#endif
import qualified Distribution.Simple.Program as Program
import qualified Distribution.Simple.GHC as GHC(configure)
#if MIN_VERSION_Cabal(2, 0, 0)
import qualified Distribution.Types.LocalBuildInfo as Distribution
#else
import Distribution.Simple.LocalBuildInfo (componentBuildInfo)
#endif
import qualified Distribution.Verbosity as Distribution

import System.IO.Error (ioeGetErrorString)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeDirectory, splitFileName, (</>))

readGenericPackageDescription :: Distribution.Verbosity -> FilePath -> IO Distribution.GenericPackageDescription
#if MIN_VERSION_Cabal(2, 0, 0)
readGenericPackageDescription = Distribution.readGenericPackageDescription
#else
readGenericPackageDescription = Distribution.readPackageDescription
#endif

#if MIN_VERSION_Cabal(2, 0, 0)
defaultProgramDb :: Program.ProgramDb
defaultProgramDb = Program.defaultProgramDb
#else
defaultProgramDb :: Program.ProgramConfiguration
defaultProgramDb = Program.defaultProgramConfiguration
#endif

-- TODO: Fix callsites so we don't need `allComponentsBy`. It was taken from
-- http://hackage.haskell.org/package/Cabal-1.16.0.3/docs/src/Distribution-Simple-LocalBuildInfo.html#allComponentsBy
-- since it doesn't exist in Cabal 1.18.*
--
-- | Obtains all components (libs, exes, or test suites), transformed by the
-- given function.  Useful for gathering dependencies with component context.
allComponentsBy :: PackageDescription
                -> (Component -> a)
                -> [a]
allComponentsBy pkg_descr f =
    [ f (CLib  lib) | Just lib <- [library pkg_descr]
                    , buildable (libBuildInfo lib) ]
 ++ [ f (CExe  exe) | exe <- executables pkg_descr
                    , buildable (buildInfo exe) ]
 ++ [ f (CTest tst) | tst <- testSuites pkg_descr
                    , buildable (testBuildInfo tst)]
 ++ [ f (CBench bm) | bm <- benchmarks pkg_descr
                    , buildable (benchmarkBuildInfo bm)]

stackifyFlags :: ConfigFlags -> Maybe StackConfig -> ConfigFlags
stackifyFlags cfg Nothing   = cfg
stackifyFlags cfg (Just si) = cfg { configHcPath = toFlag ghc
                                  , configHcPkg = toFlag ghcPkg
                                  , configDistPref    = toFlag dist
                                  , configPackageDBs  = pdbs
                                  }
    where
      pdbs                  = [Nothing, Just GlobalPackageDB] ++ pdbs'
      pdbs'                 = Just . SpecificPackageDB <$> stackDbs si
      dist                  = stackDist si
      ghc = stackGhcBinDir si </> "ghc"
      ghcPkg = stackGhcBinDir si </> "ghc-pkg"

-- via: https://groups.google.com/d/msg/haskell-stack/8HJ6DHAinU0/J68U6AXTsasJ
-- cabal configure --package-db=clear --package-db=global --package-db=$(stack path --snapshot-pkg-db) --package-db=$(stack path --local-pkg-db)

getPackageGhcOpts :: FilePath -> Maybe StackConfig -> [String] -> IO (Either String [String])
getPackageGhcOpts path mbStack opts =
    getPackageGhcOpts' `catch` (\e ->
        return $ Left $ "Cabal error: " ++ ioeGetErrorString (e :: IOException))
  where
    getPackageGhcOpts' :: IO (Either String [String])
    getPackageGhcOpts' = do
        genPkgDescr <- readGenericPackageDescription silent path
        distDir     <- getDistDir
        let programCfg = defaultProgramDb
        let initCfgFlags = (defaultConfigFlags programCfg)
                             { configDistPref = toFlag distDir
                             -- TODO: figure out how to find out this flag
                             , configUserInstall = toFlag True

                             -- configure with --enable-tests to include test dependencies/modules
                             , configTests = toFlag True

                             -- configure with --enable-benchmarks to include benchmark dependencies/modules
                             , configBenchmarks = toFlag True
                             }
        let initCfgFlags' = stackifyFlags initCfgFlags mbStack

        cfgFlags <- flip execStateT initCfgFlags' $ do
          let sandboxConfig = takeDirectory path </> "cabal.sandbox.config"

          exists <- lift $ doesFileExist sandboxConfig
          when exists $ do
            sandboxPackageDb <- lift $ getSandboxPackageDB sandboxConfig
            modify $ \x -> x { configPackageDBs = [Just sandboxPackageDb] }

          let cmdUI = configureCommand programCfg
          case commandParseArgs cmdUI True opts of
            CommandReadyToGo (modFlags, _) -> modify modFlags
            CommandErrors (e:_) -> error e
            _ -> return ()

        localBuildInfo <- configure (genPkgDescr, emptyHookedBuildInfo) cfgFlags
        let baseDir = fst . splitFileName $ path
        case getGhcVersion localBuildInfo  of
            Nothing -> return $ Left "GHC is not configured"
            Just ghcVersion  -> do
                let pkgDescr = localPkgDescr localBuildInfo
#if __GLASGOW_HASKELL__ < 802
                let mbLibName = pkgLibName pkgDescr
#endif
                let ghcOpts' = foldl' mappend mempty . map (getComponentGhcOptions localBuildInfo) $
                               flip allComponentsBy id pkgDescr
                    -- FIX bug in GhcOptions' `mappend`
#if MIN_VERSION_Cabal(2,4,0)
-- API Change, just for the glory of Satan:
-- Distribution.Simple.Program.GHC.GhcOptions no longer uses NubListR's
                    ghcOpts = ghcOpts' { ghcOptExtra = filter (/= "-Werror") $ ghcOptExtra ghcOpts'
#elif MIN_VERSION_Cabal(1,21,1)
-- API Change:
-- Distribution.Simple.Program.GHC.GhcOptions now uses NubListR's
--  GhcOptions { .. ghcOptPackages :: NubListR (InstalledPackageId, PackageId, ModuleRemaining) .. }
                    ghcOpts = ghcOpts' { ghcOptExtra = overNubListR (filter (/= "-Werror")) $ ghcOptExtra ghcOpts'
#endif
#if MIN_VERSION_Cabal(1,21,1)
#if __GLASGOW_HASKELL__ >= 709
                                       , ghcOptPackageDBs = sort $ nub (ghcOptPackageDBs ghcOpts')
#endif
#if __GLASGOW_HASKELL__ < 802
                                       , ghcOptPackages = overNubListR (filter (\(_, pkgId, _) -> Just (pkgName pkgId) /= mbLibName)) $ (ghcOptPackages ghcOpts')
#endif
                                       , ghcOptSourcePath = overNubListR (map (baseDir </>)) (ghcOptSourcePath ghcOpts')
                                       }
#else
--  GhcOptions { .. ghcOptPackages :: [(InstalledPackageId, PackageId)]  .. }
                let ghcOpts = ghcOpts' { ghcOptExtra = filter (/= "-Werror") $ nub $ ghcOptExtra ghcOpts'
                                       , ghcOptPackages = filter (\(_, pkgId) -> Just (pkgName pkgId) /= mbLibName) $ nub (ghcOptPackages ghcOpts')
                                       , ghcOptSourcePath = map (baseDir </>) (ghcOptSourcePath ghcOpts')
                                       }
#endif
                let hcPath = flagToMaybe . configHcPath $ configFlags localBuildInfo
                let pkgPath = flagToMaybe . configHcPkg $ configFlags localBuildInfo
                (ghcInfo, mbPlatform, _) <- GHC.configure silent hcPath pkgPath defaultProgramDb
                putStrLn $ "Configured GHC " ++ show ghcVersion
                                             ++ " " ++ show mbPlatform
#if MIN_VERSION_Cabal(1,23,2)
-- API Change:
-- Distribution.Simple.Program.GHC.renderGhcOptions now takes Platform argument
-- renderGhcOptions :: Compiler -> Platform -> GhcOptions -> [String]
                return $ case mbPlatform of
                    Just platform -> Right $ renderGhcOptions ghcInfo platform ghcOpts
                    Nothing       -> Left "GHC.configure did not return platform"
#else
#if MIN_VERSION_Cabal(1,20,0)
-- renderGhcOptions :: Compiler -> GhcOptions -> [String]
                return $ Right $ renderGhcOptions ghcInfo ghcOpts
#else
-- renderGhcOptions :: Version -> GhcOptions -> [String]
                return $ Right $ renderGhcOptions ghcVersion ghcOpts
#endif
#endif

    -- returns the right 'dist' directory in the case of a sandbox
    getDistDir = do
        let dir = takeDirectory path </> "dist"
        exists <- doesDirectoryExist dir
        if not exists then return dir else do
            contents <- getDirectoryContents dir
            return . maybe dir (dir </>) $ find ("dist-sandbox-" `isPrefixOf`) contents

#if __GLASGOW_HASKELL__ < 802
pkgLibName :: PackageDescription -> Maybe PackageName
pkgLibName pkgDescr = if hasLibrary pkgDescr
                      then Just $ pkgName . package $ pkgDescr
                      else Nothing

hasLibrary :: PackageDescription -> Bool
hasLibrary = isJust . library
#endif

getComponentGhcOptions :: LocalBuildInfo -> Component -> GhcOptions
getComponentGhcOptions lbi comp =
    foldMap (\clbi -> componentGhcOptions silent lbi bi clbi (buildDir lbi)) clbis

  where bi   = componentBuildInfo comp
#if MIN_VERSION_Cabal(2,0,0)
        clbis = Distribution.componentNameCLBIs lbi (componentName comp)
#else
        clbis = [getComponentLocalBuildInfo lbi (componentName comp)]
#endif

getGhcVersion :: LocalBuildInfo -> Maybe Version
getGhcVersion lbi = let db = withPrograms lbi
                     in do ghc <- lookupProgram (simpleProgram "ghc") db
                           programVersion ghc

getSandboxPackageDB :: FilePath -> IO PackageDB
getSandboxPackageDB sandboxPath = do
    contents <- readFile sandboxPath
    return $ SpecificPackageDB $ extractValue . parse $ contents
  where
    pkgDbKey = "package-db:"
    parse = head . filter (pkgDbKey `isPrefixOf`) . lines
    extractValue = takeWhile (`notElem` "\n\r") . dropWhile isSpace . drop (length pkgDbKey)
    
-- | looks for file matching a predicate starting from dir and going up until root
findFile :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findFile p dir = do
    allFiles <- getDirectoryContents dir
    case find p allFiles of
      Just cabalFile -> return $ Just $ dir </> cabalFile
      Nothing ->
        let parentDir = takeDirectory dir
         in if parentDir == dir
            then return Nothing
            else findFile p parentDir


findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile = findFile isCabalFile
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile path = ".cabal" `isSuffixOf` path
                    && length path > length ".cabal"
