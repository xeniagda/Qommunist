module Lib
    ( main
    ) where

import Text.Read

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Base
import Game
import EncodeGame
import Playing
import ParallelCommunication


main :: IO ()
main = do
    
    games <- atomically $ newTVar [] :: IO (TVar [Game])

    putStrLn "Started!"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 1961 iNADDR_ANY
    listen sock 2

    mainLoop sock games

mainLoop sock games = do
    (s, ip) <- accept sock
    
    tRecv <- newTVarIO []
    tSend <- newTVarIO []
    pings <- newTVarIO 0
    
    let client = Client ip pings tRecv tSend

    forkIO $ communicate games client

    parallelCommunicate s client

    mainLoop sock games

communicate :: TVar [Game] -> Client -> IO ()
communicate games client@(Client ip pigns tSend tRecv) = do

    putStrLn $ "Connected to " ++ show ip

    gamesInProgress <- (\x -> filter (\game -> not $ isAllConnected game) x) <$> readTVarIO games

    cSendLn client $ 
        B.append (B.pack "g") $ 
        B.intercalate (B.pack ",") $ map (B.pack . show . getId) gamesInProgress

    gameId <- readMaybe <$> B.unpack <$> cRecvWait client

    case gameId of
        Nothing -> do
            game' <- (makeGame 8 . getNextId) <$> readTVarIO games

            let (game, plID) = joinGame client game'

            cSendLn client $ B.pack $ show plID

            atomically $ modifyTVar games (game:)

            clientLoop games (getId game) client (WaitingForPlayers 0)

        Just gameId' -> do
            game'' <- getGame' games gameId'

            case game'' of
                Nothing -> do
                    cSendLn client $ B.pack "Game not found"
                    communicate games client
                Just game' -> do
                    if isAllConnected game'
                        then do
                            cSendLn client $ B.pack "full"
                            communicate games client
                        else do

                            let (game, plID) = joinGame client game'

                            cSendLn client $ B.pack $ show plID

                            update' games game

                            clientLoop games gameId' client (WaitingForPlayers 0)
