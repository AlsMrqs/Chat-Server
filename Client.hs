module Client where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import System.IO
import System.Process
import Data.Bool
import Data.Either

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

    tidTalker <- forkIO $ talker chat buff (sock, addr, hdl)

    reader chat buff hdl >> killThread tidTalker >> hClose hdl >> close sock 
    putStrLn "The server is closed!"

interface :: MVar [Char] -> MVar [Char] -> IO ()
interface chat buff = do
    chat' <- readMVar chat
    buff' <- readMVar buff
    system "clear" >> putStr (chat' ++ buff') 

reader :: MVar [Char] -> MVar [Char] -> Handle -> IO ()
reader chat buff hdl = do 
    result <- try (hGetLine hdl) :: IO (Either IOException String)
    case result of
        Right msg -> do
            modifyMVar_ chat (evaluate . (++ (msg ++ "\n"))) 
            interface chat buff >> reader chat buff hdl
        Left  _   -> return ()

sender :: Handle -> [Char] -> IO (Either IOException ())
sender hdl = try . hPutStrLn hdl 

talker :: MVar [Char] -> MVar [Char] -> DBMS.Host -> IO ()
talker chat buff (sock, addr, hdl) = do
    res <- sender hdl . (++) (show addr ++ ": ") =<< writer chat buff
    if isRight res
        then talker chat buff (sock, addr, hdl)
        else return ()

writer :: MVar [Char] -> MVar [Char] -> IO [Char]
writer chat buff = do
    input <- getChar
    if (not . (==) '\n') input 
        then manage buff input >> interface chat buff >> writer chat buff
        else swapMVar buff []

manage :: MVar [Char] -> Char -> IO ()
manage buff input = do
    case input of
        '\DEL' -> do modifyMVar_ buff (evaluate . (\x -> bool (init x) [] $ null x)) 
        _      -> do modifyMVar_ buff (evaluate . (++ [input])) 

