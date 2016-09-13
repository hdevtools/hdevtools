module Types
    ( ServerDirective(..)
    , ClientDirective(..)
    , Command(..)
    , CommandExtra(..)
    , emptyCommandExtra
    ) where

import System.Exit (ExitCode)

data CommandExtra = CommandExtra
  { cePath :: Maybe FilePath
  , ceGhcOptions :: [String]
  , ceCabalFilePath :: Maybe FilePath
  , ceCabalOptions :: [String]
  , ceStackYamlPath :: Maybe FilePath
  , ceTemplateHaskell :: Bool
  } deriving (Read, Show)

emptyCommandExtra :: CommandExtra
emptyCommandExtra = CommandExtra { cePath = Nothing
                                 , ceGhcOptions  = []
                                 , ceCabalFilePath = Nothing
                                 , ceCabalOptions = []
                                 , ceStackYamlPath = Nothing
                                 , ceTemplateHaskell = True
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
    | CmdFindSymbol String [String]
    deriving (Read, Show)
