{-# LANGUAGE CPP #-}
module Stack
      ( -- * The bits of information needed from `stack`
        StackConfig (..)
      , findStackYaml
      , getStackConfig
      ) where

import Data.Char (isSpace)

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative((<$>), (<*>))
import System.IO
#endif

import System.Process
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Control.Exception
import Types

-- TODO: Move into Types?
data StackConfig = StackConfig { stackYaml :: FilePath
                               , stackDist :: FilePath
                               , stackDbs  :: [FilePath]
                               , stackGhcBinDir :: FilePath
                               , stackGhcLibDir :: FilePath
                               }
                   deriving (Eq, Show)

-- | Search for a @stack.yaml@ upwards in given file path tree.
findStackYaml :: FilePath -> IO (Maybe FilePath)
findStackYaml = fmap (fmap trim) . execStackInPath "path --config-location"

-- | Run @stack path@ to compute @StackConfig@
getStackConfig :: CommandExtra -> IO (Maybe StackConfig)
getStackConfig CommandExtra { ceStackYamlPath = Nothing } = return Nothing
getStackConfig CommandExtra { ceStackYamlPath = Just p } = do
    dbs <- getStackDbs root
    dist <- getStackDist root
    ghcBinDir <- getStackGhcBinDir root
    ghcLibDir <- getStackGhcLibDir root
    return $ StackConfig p <$> dist
                           <*> dbs
                           <*> ghcBinDir
                           <*> ghcLibDir
  where
    root = takeDirectory p

getStackGhcBinDir :: FilePath -> IO (Maybe FilePath)
getStackGhcBinDir = fmap (fmap trim) . execStackInPath "path --compiler-bin"

getStackGhcLibDir :: FilePath -> IO (Maybe FilePath)
getStackGhcLibDir p = do
    ghc <- (trim <$>) <$> execStackInPath "path --compiler-exe" p
    case ghc of
        Just exe -> (trim <$>) <$> execInPath (exe ++ " --print-libdir") p
        Nothing -> return Nothing

getStackDist :: FilePath -> IO (Maybe FilePath)
getStackDist p = (trim <$>) <$> execStackInPath "path --dist-dir" p

getStackDbs :: FilePath -> IO (Maybe [FilePath])
getStackDbs p =
  execStackInPath "path --ghc-package-path" p >>=
  maybe (return Nothing) (\pp -> return <$> extractDbs pp)

extractDbs :: String -> IO [FilePath]
extractDbs = filterM doesDirectoryExist . stringPaths

stringPaths :: String -> [String]
stringPaths = splitBy ':' . trim

--------------------------------------------------------------------------------
-- | Generic Helpers
--------------------------------------------------------------------------------

splitBy :: Char -> String -> [String]
splitBy c str
  | null str' = [x]
  | otherwise = x : splitBy c (tail str')
  where
    (x, str') = span (c /=) str

trim :: String -> String
trim = f . f
   where
     f = reverse . dropWhile isSpace

-- Execute stack command in path (if stack is available)
execStackInPath :: String -> FilePath -> IO (Maybe String)
execStackInPath a p =
  findExecutable "stack" >>= maybe (return Nothing) (const $ execInPath ("stack " ++ a) p)

#if __GLASGOW_HASKELL__ < 709
execInPath :: String -> FilePath -> IO (Maybe String)
execInPath cmd p = do
    eIOEstr <- try $ createProcess prc :: IO (Either IOError ProcH)
    case eIOEstr of
        Right (_, Just h, _, _)  -> Just <$> getClose h
        Right (_, Nothing, _, _) -> return Nothing
        -- This error is most likely "/bin/sh: stack: command not found"
        -- which is caused by the package containing a stack.yaml file but
        -- no stack command is in the PATH.
        Left _  -> return Nothing
  where
    prc          = (shell cmd) { cwd = Just $ takeDirectory p }

getClose :: Handle -> IO String
getClose h = do
  str <- hGetContents h
  hClose h
  return str

type ProcH = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- Not deleting this because this is likely more robust than the above! (but
-- only works on process-1.2.3.0 onwards

#else
execInPath :: String -> FilePath -> IO (Maybe String)
execInPath cmd p = do
    eIOEstr <- try $ readCreateProcess prc "" :: IO (Either IOError String)
    return $ case eIOEstr of
        Right s -> Just s
        -- This error is most likely "/bin/sh: stack: command not found"
        -- which is caused by the package containing a stack.yaml file but
        -- no stack command is in the PATH.
        Left _  -> Nothing
  where
    prc          = (shell cmd) { cwd = Just p }
#endif
