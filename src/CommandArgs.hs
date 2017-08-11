{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module CommandArgs
    ( HDevTools(..)
    , loadHDevTools
    , pathArg
    )
where

import Cabal (findFile)
import Data.Version (showVersion)
import Paths_hdevtools (version)
import qualified Config
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit (splitArgs)
import System.Directory (getCurrentDirectory)
import System.Environment (getProgName, withArgs, getArgs)
import System.FilePath (takeDirectory)
import System.Info (arch, os)

programVersion :: String
programVersion =
    "version " ++ showVersion version

cabalVersion :: String
cabalVersion =
    "cabal-" ++ VERSION_Cabal

fullVersion :: String
fullVersion =
    concat
        [ programVersion
        , " ("
        , "ghc-", Config.cProjectVersion, "-", arch, "-", os
        , ", ", cabalVersion
        , ")"
        ]

data HDevTools
    = Admin
        { socket       :: Maybe FilePath
        , ghcOpts :: [String]
        , start_server :: Bool
        , cabalOpts :: [String]
        , noDaemon     :: Bool
        , status       :: Bool
        , stop_server  :: Bool
        , debug :: Bool
        , noStack :: Bool
        }
    | Check
        { socket  :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
        , path    :: Maybe String
        , file    :: String
        , json    :: Bool
        , debug :: Bool
        , noStack :: Bool
        , noTH :: Bool
        }
    | ModuleFile
        { socket  :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
        , module_ :: String
        , debug :: Bool
        , noStack :: Bool
        }
    | Info
        { socket     :: Maybe FilePath
        , ghcOpts    :: [String]
        , cabalOpts  :: [String]
        , path       :: Maybe String
        , file       :: String
        , identifier :: String
        , debug :: Bool
        , noStack :: Bool
        , noTH :: Bool
        }
    | Type
        { socket  :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts  :: [String]
        , path    :: Maybe String
        , file    :: String
        , line    :: Int
        , col     :: Int
        , debug :: Bool
        , noStack :: Bool
        , noTH :: Bool
        }
    | FindSymbol
        { socket :: Maybe FilePath
        , ghcOpts :: [String]
        , cabalOpts :: [String]
        , symbol :: String
        , files :: [String]
        , debug :: Bool
        , noStack :: Bool
        , noTH :: Bool
        }
    deriving (Show, Data, Typeable)

dummyAdmin :: HDevTools
dummyAdmin = Admin
    { socket       = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , start_server = False
    , noDaemon     = False
    , status       = False
    , stop_server  = False
    , debug = False
    , noStack = False
    }

dummyCheck :: HDevTools
dummyCheck = Check
    { socket  = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , path    = Nothing
    , file    = ""
    , json    = False
    , debug = False
    , noTH = False
    , noStack = False
    }

dummyModuleFile :: HDevTools
dummyModuleFile = ModuleFile
    { socket  = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , module_ = ""
    , debug = False
    , noStack = False
    }

dummyInfo :: HDevTools
dummyInfo = Info
    { socket     = Nothing
    , ghcOpts    = []
    , cabalOpts = []
    , path       = Nothing
    , file       = ""
    , identifier = ""
    , debug = False
    , noStack = False
    , noTH = False
    }

dummyType :: HDevTools
dummyType = Type
    { socket  = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , path    = Nothing
    , file    = ""
    , line    = 0
    , col     = 0
    , debug = False
    , noStack = False
    , noTH = False
    }

dummyFindSymbol :: HDevTools
dummyFindSymbol = FindSymbol
    { socket = Nothing
    , ghcOpts = []
    , cabalOpts = []
    , symbol = ""
    , files = []
    , debug = False
    , noStack = False
    , noTH = False
    }

admin :: Annotate Ann
admin = record dummyAdmin
    [ socket       := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , start_server := def            += help "start server"
    , noDaemon     := def            += help "do not daemonize (only if --start-server)"
    , status       := def            += help "show status of server"
    , stop_server  := def            += help "shutdown the server"
    , debug    := def                += help "enable debug output"
    , noStack  := def += name "S"    += help "disable stack integration"
    ] += help "Interactions with the server"

check :: Annotate Ann
check = record dummyCheck
    [ socket   := def += typFile      += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , path     := def += typFile      += help "path to target file"
    , file     := def += typFile      += argPos 0 += opt ""
    , json     := def                 += help "render output as JSON"
    , debug    := def                 += help "enable debug output"
    , noStack  := def += name "S"     += help "disable stack integration"
    , noTH     := def                 += help "disable template haskell"
    ] += help "Check a haskell source file for errors and warnings"

moduleFile :: Annotate Ann
moduleFile = record dummyModuleFile
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , module_  := def += typ "MODULE" += argPos 0
    , debug    := def                 += help "enable debug output"
    , noStack  := def += name "S"     += help "disable stack integration"
    ] += help "Get the haskell source file corresponding to a module name"

info :: Annotate Ann
info = record dummyInfo
    [ socket     := def += typFile      += help "socket file to use"
    , ghcOpts    := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , path       := def += typFile      += help "path to target file"
    , file       := def += typFile      += argPos 0 += opt ""
    , identifier := def += typ "IDENTIFIER" += argPos 1
    , debug      := def                 += help "enable debug output"
    , noStack  := def += name "S"       += help "disable stack integration"
    , noTH     := def                 += help "disable template haskell"
    ] += help "Get info from GHC about the specified identifier"

type_ :: Annotate Ann
type_ = record dummyType
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , debug    := def                 += help "enable debug output"
    , noStack  := def += name "S"     += help "disable stack integration"
    , path     := def += typFile      += help "path to target file"
    , file     := def += typFile      += argPos 0 += opt ""
    , line     := def += typ "LINE"   += argPos 1
    , col      := def += typ "COLUMN" += argPos 2
    , noTH     := def                 += help "disable template haskell"
    ] += help "Get the type of the expression at the specified line and column"

findSymbol :: Annotate Ann
findSymbol = record dummyFindSymbol
    [ socket   := def += typFile += help "socket file to use"
    , ghcOpts  := def += typ "OPTION" += help "ghc options"
    , cabalOpts := def += typ "OPTION"  += help "cabal options"
    , symbol   := def += typ "SYMBOL" += argPos 0
    , files    := def += typFile += args
    , debug    := def                 += help "enable debug output"
    , noStack  := def += name "S"     += help "disable stack integration"
    , noTH     := def                 += help "disable template haskell"
    ] += help "List the modules where the given symbol could be found"

full :: String -> Annotate Ann
full progName = modes_ [admin += auto, check, moduleFile, info, type_, findSymbol]
        += helpArg [name "h", groupname "Help"]
        += versionArg [groupname "Help"]
        += program progName
        += summary (progName ++ ": " ++ fullVersion)


fileArg :: HDevTools -> Maybe String
fileArg (Admin {})      = Nothing
fileArg (ModuleFile {}) = Nothing
fileArg a@(Check {}) = Just $ file a
fileArg a@(Info  {}) = Just $ file a
fileArg a@(Type  {}) = Just $ file a
fileArg (FindSymbol {}) = Nothing

pathArg' :: HDevTools -> Maybe String
pathArg' (Admin {})      = Nothing
pathArg' (ModuleFile {}) = Nothing
pathArg' a@(Check {}) = path a
pathArg' a@(Info  {}) = path a
pathArg' a@(Type  {}) = path a
pathArg' (FindSymbol {}) = Nothing

pathArg :: HDevTools -> Maybe String
pathArg a = case pathArg' a of
                Just x  -> Just x
                Nothing -> fileArg a


loadHDevTools :: IO HDevTools
loadHDevTools = do
    progName <- getProgName
    cfg0 <- cmdArgs_ (full progName)
    dir  <- maybe getCurrentDirectory (return . takeDirectory) $ pathArg cfg0
    mConfig <- findFile (== ".hdevtoolsrc") dir
    perProject <- maybe (return []) (\f -> splitArgs `fmap` readFile f) mConfig
    args0 <- getArgs
    withArgs (args0 ++ perProject) $ cmdArgs_ (full progName)
