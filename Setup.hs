module Setup where

import Control.Concurrent.MVar
import Network.Socket
import System.IO
import Data.Bool

import qualified DBMS.Engine as DBMS

interface :: MVar [DBMS.Host] -> IO ()
interface link = do
    input <- putStr "server> " >> getLine
    case input of
        "whoami" -> whoami link >> interface link
        "close"  -> do
            putStrLn "Closing!"
            mapM_ disconnect . tail =<< takeMVar link
            putStrLn "mapM_ done!"
            return ()
        _        -> putStrLn input >> interface link

whoami :: MVar [DBMS.Host] -> IO ()
whoami link = print . getAddr . head =<< readMVar link
    where
        getAddr (_,addr,_) = addr

disconnect :: DBMS.Host -> IO ()
disconnect (sock,_,hdl) = hClose hdl >> close sock

