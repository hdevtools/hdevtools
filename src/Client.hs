module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import           Control.Exception (bracket, tryJust)
import           Control.Monad     (guard)
import           Daemonize         (daemonize)
import           Network.Socket    (Family (AF_UNIX), SockAddr (SockAddrUnix),
                                    SocketType (Stream), connect,
                                    defaultProtocol, socket, socketToHandle)
import           Server            (createListenSocket, startServer)
import           System.Exit       (exitFailure, exitWith)
import           System.IO         (Handle, IOMode (ReadWriteMode), hClose,
                                    hFlush, hGetLine, hPrint, hPutStrLn, stderr)
import           System.IO.Error   (isDoesNotExistError)
import           Types             (ClientDirective (..), Command (..),
                                    CommandExtra (..), ServerDirective (..))
import           Util              (readMaybe)

connectSocket :: FilePath -> IO Handle
connectSocket sock = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix sock)
  socketToHandle s ReadWriteMode

getServerStatus :: FilePath -> IO ()
getServerStatus sock =
    bracket (connectSocket sock) hClose $ \h -> do
      hPrint h SrvStatus
      hFlush h
      startClientReadLoop h

stopServer :: FilePath -> IO ()
stopServer sock = do
    bracket (connectSocket sock) hClose $ \h -> do
      hPrint h SrvExit
      hFlush h
      startClientReadLoop h

serverCommand :: FilePath -> Command -> CommandExtra -> IO ()
serverCommand sock cmd cmdExtra = do
    r <- tryJust (guard . isDoesNotExistError) (connectSocket sock)
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
