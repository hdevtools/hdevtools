module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Network (PortID(UnixSocket), connectTo)
import System.Exit (exitFailure, exitWith)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn, hPrint, stderr)
import System.IO.Error (isDoesNotExistError)

import Daemonize (daemonize)
import Server (createListenSocket, startServer)
import Types (ClientDirective(..), Command(..), CommandExtra(..), ServerDirective(..))
import Util (readMaybe)

connect :: FilePath -> IO Handle
connect sock = 
  connectTo "" (UnixSocket sock)

getServerStatus :: FilePath -> IO ()
getServerStatus sock = do
    h <- connect sock
    hPrint h SrvStatus
    hFlush h
    startClientReadLoop h

stopServer :: FilePath -> IO ()
stopServer sock = do
    h <- connect sock
    hPrint h SrvExit
    hFlush h
    startClientReadLoop h

serverCommand :: FilePath -> Command -> CommandExtra -> IO ()
serverCommand sock cmd cmdExtra = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPrint h (SrvCommand cmd cmdExtra)
            hFlush h
            startClientReadLoop h
        Left _ -> do
            s <- createListenSocket sock
            daemonize False $ startServer sock (Just s) cmdExtra
            serverCommand sock cmd cmdExtra

startClientReadLoop :: Handle -> IO ()
startClientReadLoop h = do
    msg <- hGetLine h
    let clientDirective = readMaybe msg
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> startClientReadLoop h
        Just (ClientStderr err) -> hPutStrLn stderr err >> startClientReadLoop h
        Just (ClientExit exitCode) -> hClose h >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $
                "The server sent an invalid message to the client: " ++ show msg

unexpectedError :: String -> IO ()
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
