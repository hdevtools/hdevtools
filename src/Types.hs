module Types
    ( ServerDirective(..)
    , ClientDirective(..)
    , Command(..)
    , CommandExtra(..)
    , emptyCommandExtra
    , debug
    ) where

import System.Exit (ExitCode)

data CommandExtra = CommandExtra
  { ceGhcOptions :: [String]
  , ceCabalConfig :: Maybe FilePath
  } deriving (Read, Show)

emptyCommandExtra :: CommandExtra
emptyCommandExtra = CommandExtra { ceGhcOptions = []
                                 , ceCabalConfig = Nothing
                                 }

data ServerDirective
    = SrvCommand Command CommandExtra
    | SrvStatus
    | SrvExit
    deriving (Read, Show)

data ClientDirective
    = ClientStdout String
    | ClientStderr String
    | ClientExit ExitCode
    | ClientUnexpectedError String -- ^ For unexpected errors that should not happen
    deriving (Read, Show)

data Command
    = CmdCheck FilePath
    | CmdModuleFile String
    | CmdInfo FilePath String
    | CmdType FilePath (Int, Int)
    deriving (Read, Show)


debug :: String -> IO ()
debug msg = appendFile "/Users/rjhala/tmp/hdevtools-debug-master" $ msg ++ "\n"
