module DBMS.Engine where

import Control.Concurrent.MVar
import Control.Exception
import Network.Socket
import System.IO
import Data.List
import Data.Bool
import Data.Time.Clock.POSIX

type Host = (Socket, SockAddr, Handle) 
type Msg = [Char]

getHandle :: Host -> Handle
getHandle (_,_,x) = x

record :: MVar [a] -> a -> IO ()
record s x = putMVar s . (++ [x]) =<< takeMVar s

forget :: Eq a => MVar [a] -> a -> IO [a]
forget s x = eval . partition (== x) =<< takeMVar s
    where
        eval = \(a,b) -> putMVar s b >> return a

time :: IO Double
time = return . realToFrac =<< getPOSIXTime

