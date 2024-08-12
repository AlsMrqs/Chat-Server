module Manager where

import Control.Concurrent.MVar
import Control.Concurrent
import Network.Socket
import System.IO

import Initializer
import Setup

import qualified DBMS.Engine as DBMS

manage :: MVar [DBMS.Msg] -> MVar [DBMS.Host] -> Socket -> IO ()
manage chat link sock = do
    host <- newClient sock
    DBMS.record link host
    
    forkIO (initialize link host chat "Sigma Nature!")

    manage chat link sock

newClient :: Socket -> IO (Socket, SockAddr, Handle)
newClient sock = do
    (cliSock, cliAddr) <- accept sock
    cliHandle <- socketToHandle cliSock ReadWriteMode
    return (cliSock, cliAddr, cliHandle) 

