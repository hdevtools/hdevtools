module Stack (debug, getStackDbs) where

import Data.Char (isSpace)
import System.Process
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Types

-- 1. Figure out if this is a stack project,
-- 2. Run stack exec ... to extract path

debug :: String -> IO ()
debug msg = appendFile "/Users/rjhala/tmp/hdevtools-debug" $ msg ++ "\n"

getStackDbs :: CommandExtra -> IO (Maybe [FilePath])
getStackDbs ce = do
  r <- getStackDbs_ ce
  debug $ "CommandExtra: " ++ show ce
  debug $ "Result: " ++ show r
  return r


getStackDbs_ :: CommandExtra -> IO (Maybe [FilePath])
getStackDbs_ ce = case cePath ce of
                   Nothing -> return Nothing
                   Just p  -> getStackDbs' p

getStackDbs' :: FilePath -> IO (Maybe [FilePath])
getStackDbs' p = do
  b <- isStackProject p
  if b
    then Just <$> pathStackDbs p
    else return Nothing

isStackProject :: FilePath -> IO Bool
isStackProject p = existsM doesFileExist paths
  where
    paths        = [ d </> "stack.yaml" | d <- pathsToRoot dir]
    dir          = takeDirectory p

pathsToRoot :: FilePath -> [FilePath]
pathsToRoot p
  | p == parent = [p]
  | otherwise   = p : pathsToRoot parent
  where
    parent      = takeDirectory p

existsM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
existsM f xs = (not . null) <$> filterM f xs

pathStackDbs :: FilePath -> IO [FilePath]
pathStackDbs p = readCreateProcess prc "" >>= extractDbs
  where
    prc        = (shell cmd) { cwd = Just $ takeDirectory p }
    cmd        = "stack --verbosity quiet exec printenv GHC_PACKAGE_PATH"

extractDbs :: String -> IO [FilePath]
extractDbs = filterM doesDirectoryExist . stringPaths

stringPaths :: String -> [String]
stringPaths = splitBy ':' . trim

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
