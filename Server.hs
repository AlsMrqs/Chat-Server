module Server where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.IO

import Manager 
import Setup

import qualified DBMS.Engine as DBMS

main :: IO ()
main = start =<< newMVar []

start :: MVar [DBMS.Host] -> IO ()
start link = do
    sock <- newServerSocket
    addr <- getSocketName sock
    hdl  <- openFile "./temp.data" ReadWriteMode
    DBMS.record link (sock, addr, hdl)

    msg <- newMVar [] :: IO (MVar [DBMS.Msg]) 
    tid <- forkIO (manage msg link sock)

    interface link
    killThread tid  >> putStrLn "The (Manager) was closed!"
    hClose     hdl  >> putStrLn "The (Log|Handle) was closed!"
    close      sock >> putStrLn "The (Server Socket) was closed!"

newServerSocket :: IO Socket
newServerSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet 4245 $ tupleToHostAddress (127, 0, 0, 1))
    listen sock 5
    return sock

