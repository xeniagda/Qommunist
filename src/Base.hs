module Base where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

second :: Int
second = 1000 * 1000

setAt :: [a] -> Int -> a -> [a]
setAt (a:b) 0 elem = elem : b
setAt (a:b) n elem = a : setAt b (n-1) elem

data Client = 
    Client 
        SockAddr -- IP
        (TVar Int)            -- Pings dropped
        (TVar [B.ByteString]) -- Stack of messages received
        (TVar [B.ByteString]) -- Stack of messages to send
    deriving (Eq)

instance Show Client where
    show (Client ip _ _ _) = "Client " ++ show ip

cSendLn :: Client -> B.ByteString -> IO ()
cSendLn cl x =
    cSend cl (B.snoc x '\n')

cSend (Client _ _ _ m) x =
    atomically $ modifyTVar m (++ [x])


cRecv :: Client -> IO (Maybe B.ByteString)
cRecv (Client _ _ var _) = do
    messages <- readTVarIO var
    case messages of
        [] -> return Nothing
        a:rst -> do
            atomically $ writeTVar var rst
            return $ Just a

cRecvWait :: Client -> IO B.ByteString
cRecvWait cl@(Client _ _ var _) = do
    messages <- readTVarIO var
    case messages of
        [] -> do
            threadDelay 10000
            cRecvWait cl
        a:rst -> do
            atomically $ writeTVar var rst
            return a
