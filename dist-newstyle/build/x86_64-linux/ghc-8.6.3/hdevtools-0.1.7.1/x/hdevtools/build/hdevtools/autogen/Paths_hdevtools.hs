{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hdevtools (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,7,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jrp/.cabal/bin"
libdir     = "/home/jrp/.cabal/lib/x86_64-linux-ghc-8.6.3/hdevtools-0.1.7.1-inplace-hdevtools"
dynlibdir  = "/home/jrp/.cabal/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/jrp/.cabal/share/x86_64-linux-ghc-8.6.3/hdevtools-0.1.7.1"
libexecdir = "/home/jrp/.cabal/libexec/x86_64-linux-ghc-8.6.3/hdevtools-0.1.7.1"
sysconfdir = "/home/jrp/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hdevtools_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hdevtools_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hdevtools_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hdevtools_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hdevtools_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hdevtools_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
