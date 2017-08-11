module ParallelCommunication where

import Control.Monad -- For when
import Control.Exception -- For catch
import Control.Concurrent -- For threadDelay
import Control.Concurrent.STM -- For atomically
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as B

import Base
import Game

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString


parallelCommunicate :: Socket -> Client -> IO ()
parallelCommunicate sock client@(Client ip tPings tRecv tSend) = do
    forkIO $ parallelSend sock client
    forkIO $ parallelRecv sock client
    forkIO $ parallelPing sock client
    return ()

parallelSend :: Socket -> Client -> IO ()
parallelSend sock client@(Client ip tPings tRecv tSend) = do

    pings <- readTVarIO tPings

    unless (pings > pingLimit) $ do

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
parallelRecv sock client@(Client ip tPings tRecv tSend) = do

    pings <- readTVarIO tPings

    unless (pings > pingLimit) $ do

        msg <- recv sock 1000 `catch` catchRet B.empty

        let lines = B.split '\n' msg

        sequence_ $
            map (\line -> atomically $
                if line == B.pack "pong"
                    then writeTVar tPings 0
                    else 
                        if line == B.pack ""
                            then return ()
                            else modifyTVar tRecv (++ [line])
            ) lines

        parallelRecv sock client

parallelPing :: Socket -> Client -> IO ()
parallelPing sock client@(Client ip tPings tRecv tSend) = do

    pings <- readTVarIO tPings
    
    unless (pings > pingLimit) $ do

        cSendLn client $ B.pack "ping"

        atomically $ modifyTVar tPings (+1)
        
        threadDelay second

        parallelPing sock client

catchRet :: x -> IOException -> IO x
catchRet x _ = return x
