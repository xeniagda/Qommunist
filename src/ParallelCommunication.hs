module ParallelCommunication where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as B

import Base

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString


parallelCommunicate :: Socket -> Client -> IO ()
parallelCommunicate sock client@(Client ip droppedPings tRecv tSend) = do
    forkIO $ parallelSend sock client
    forkIO $ parallelRecv sock client
    forkIO $ parallelPing sock client
    return ()

parallelSend :: Socket -> Client -> IO ()
parallelSend sock client@(Client ip droppedPings tRecv tSend) = do
    newMessages <- readTVarIO tSend
    case newMessages of
        [] -> return ()
        msg:rst -> do
            send sock msg `catch` catchRet 0
            atomically $ writeTVar tSend rst

    threadDelay (second `quot` 100)
    parallelSend sock client

-- TODO: Might want to consider lines longer than 1000 chars for some reason
parallelRecv :: Socket -> Client -> IO ()
parallelRecv sock client@(Client ip droppedPings tRecv tSend) = do
    msg <- recv sock 1000 `catch` catchRet B.empty

    let lines = B.split '\n' msg
    dropped <- readTVarIO droppedPings

    sequence_ $
        map (\line -> atomically $
            if line == B.pack "pong"
                then writeTVar droppedPings 0
                else 
                    if line == B.pack ""
                        then return ()
                        else modifyTVar tRecv (++ [line])
        ) lines

    parallelRecv sock client

parallelPing :: Socket -> Client -> IO ()
parallelPing sock client@(Client ip droppedPings tRecv tSend) = do
    cSendLn client $ B.pack "ping"

    atomically $ modifyTVar droppedPings (+1)
    
    threadDelay second

    parallelPing sock client

catchRet :: x -> IOException -> IO x
catchRet x _ = return x
