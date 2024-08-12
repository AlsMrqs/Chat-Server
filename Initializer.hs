module Initializer where 

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.IO
import Data.Either

import qualified DBMS.Engine as DBMS

initialize :: MVar [DBMS.Host] -> DBMS.Host -> MVar [DBMS.Msg] -> String -> IO ()
initialize link (sock, addr, hdl) chat str = do
    result <- try (hPutStrLn hdl str) :: IO (Either IOException ())
    if isLeft result 
        then killThread =<< myThreadId
        else do
            tidReader <- forkIO $ reader link hdl
            ping hdl >> killThread tidReader
            DBMS.forget link (sock, addr, hdl) 
            killThread =<< myThreadId

ping :: Handle -> IO ()
ping hdl = do
    result <- try (hPutStr hdl "") :: IO (Either IOException ())
    case result of
        Left  _ -> return ()
        Right _ -> threadDelay 500000 >> ping hdl

reader :: MVar [DBMS.Host] -> Handle -> IO ()
reader link hdl = do
    result <- try (hGetLine hdl) :: IO (Either IOException [Char])
    case result of
        Left  _   -> return ()
        Right msg -> readMVar link >>= broadcast msg . tail >> reader link hdl

broadcast :: String -> [DBMS.Host] -> IO ()
broadcast str [] = return ()
broadcast str ((sock, addr, hdl):ls) = do
    try (hPutStrLn hdl str) :: IO (Either IOException ())
    broadcast str ls

