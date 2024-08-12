module Client where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import System.IO
import System.Process

import Network.Socket

import qualified DBMS.Engine as DBMS

main :: IO ()
main = do
    let serverAddr = SockAddrInet 4245 (tupleToHostAddress (127, 0, 0, 1))
    sock   <- socket AF_INET Stream defaultProtocol
    result <- try (connect sock serverAddr) :: IO (Either IOException ())
    case result of
        Left  _ -> putStrLn "Server offline!"
        Right _ -> start =<< newHost sock

newHost :: Socket -> IO DBMS.Host
newHost sock = do
    addr <- getSocketName  sock 
    hdl  <- socketToHandle sock ReadWriteMode
    return (sock, addr, hdl)

start :: DBMS.Host -> IO ()
start (sock, addr, hdl) = do
    hSetBuffering hdl NoBuffering
    hSetEcho stdin False

    buff <- newMVar [] :: IO (MVar [Char])
    chat <- newMVar [] :: IO (MVar [Char])

    tidInterface <- forkIO $ interface chat buff
    tidTalker    <- forkIO $ talker chat buff (sock, addr, hdl)

    reader chat hdl
    killThread tidTalker
    hClose hdl
    close  sock
    killThread tidInterface
    putStrLn "The server is closed!"

interface :: MVar [Char] -> MVar [Char] -> IO ()
interface chat buff = do
    chat' <- readMVar chat
    buff' <- readMVar buff
    system "clear" >> putStr (chat' ++ buff') >> threadDelay 200000 
    interface  chat buff

reader :: MVar [Char] -> Handle -> IO ()
reader chat hdl = do 
    result <- try (hGetLine hdl) :: IO (Either IOException String)
    case result of
        Left  _   -> return ()
        Right msg -> modifyMVar_ chat (evaluate . (++ (msg ++ "\n"))) >> reader chat hdl

talker :: MVar [Char] -> MVar [Char] -> DBMS.Host -> IO ()
talker chat buff (sock, addr, hdl) = do
    let prefix = (++) (show addr) ": "
    msg    <- return . (++) prefix =<< writer buff 
    result <- try (hPutStrLn hdl msg) :: IO (Either IOException ())
    case result of
        Left  _ -> return ()
        Right _ -> talker chat buff (sock, addr, hdl)

writer :: MVar [Char] -> IO [Char]
writer buff = do
    input <- getChar
    case input of
        '\n' -> swapMVar buff []
        _    -> modifyMVar_ buff (evaluate . (++ [input])) >> writer buff

