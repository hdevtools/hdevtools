{-# LANGUAGE CPP #-}
module Stack
      ( -- * The bits of information needed from `stack`
        StackConfig (..)
        -- * Run `stack exec` to compute @StackConfig@
      , getStackConfig
      ) where

import Data.Maybe (listToMaybe)
import Data.Char (isSpace)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative((<$>), (<*>))
#endif
import System.Process
import System.FilePath
import System.Directory
import Control.Monad (filterM)
import Types

-- | This module adds support for `stack`, as follows:
--   1. Figure out if the target-file is in a stack project,
--   2. Run `stack exec` to extract `StackConfig`
--   3. The `StackConfig` is used to suitably alter the cabal ConfigFlags in Cabal.hs


-- TODO: Move into Types?
data StackConfig = StackConfig { stackDist :: FilePath
                               , stackDbs  :: [FilePath]
                               }
                   deriving (Eq, Show)

--------------------------------------------------------------------------------
getStackConfig :: CommandExtra -> IO (Maybe StackConfig)
--------------------------------------------------------------------------------
getStackConfig ce = case cePath ce of
                      Nothing -> return Nothing
                      Just p  -> getStackConfig' p

getStackConfig' :: FilePath -> IO (Maybe StackConfig)
getStackConfig' p = do
  mbYaml <- getStackYaml p
  case mbYaml of
    Nothing -> return Nothing
    Just _  -> jStackConfig <$> getStackDist p <*> getStackDbs p
  where
    jStackConfig x y = Just (StackConfig x y)

--------------------------------------------------------------------------------
getStackYaml :: FilePath -> IO (Maybe FilePath)
--------------------------------------------------------------------------------
getStackYaml p = listToMaybe <$> filterM doesFileExist paths
  where
    paths      = [ d </> "stack.yaml" | d <- pathsToRoot dir]
    dir        = takeDirectory p

pathsToRoot :: FilePath -> [FilePath]
pathsToRoot p
  | p == parent = [p]
  | otherwise   = p : pathsToRoot parent
  where
    parent      = takeDirectory p

--------------------------------------------------------------------------------
getStackDist :: FilePath -> IO FilePath
--------------------------------------------------------------------------------
getStackDist p = trim <$> execInPath cmd p
  where
    cmd        = "stack path --dist-dir"
    -- dir        = takeDirectory p
    -- splice     = (dir </>) . trim

--------------------------------------------------------------------------------
getStackDbs :: FilePath -> IO [FilePath]
--------------------------------------------------------------------------------
getStackDbs p = execInPath cmd p >>= extractDbs
  where
    cmd       = "stack --verbosity quiet exec printenv GHC_PACKAGE_PATH"

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

execInPath :: String -> FilePath -> IO String
execInPath cmd p = readCreateProcess prc ""
  where
    prc          = (shell cmd) { cwd = Just $ takeDirectory p }
