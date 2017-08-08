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
    let client = Client s ip

    forkIO $ communicate games client

    mainLoop sock games

communicate :: TVar [Game] -> Client -> IO ()
communicate games (Client csock ip) = do

    putStrLn $ "Connected to " ++ show ip

    gamesInProgress <- (\x -> filter (\game -> not $ isAllConnected game) x) <$> readTVarIO games

    sendLn csock $ 
        B.append (B.pack "G") $ 
        B.intercalate (B.pack ",") $ map (B.pack . show . getId) gamesInProgress

    gameId <- readMaybe <$> B.unpack <$> recv csock 100
    
    case gameId of
        Nothing -> do
            game' <- (makeGame 8 . getNextId) <$> readTVarIO games

            let (game, plID) = joinGame (Client csock ip) game'

            sendLn csock $ B.pack $ show plID

            atomically $ modifyTVar games (game:)

            putStrLn $ show ip ++ " made a new game!"
            putStrLn $ show game

            clientLoop games (getId game) (Client csock ip) (WaitingForPlayers 0)

        Just gameId' -> do
            game'' <- getGame' games gameId'

            case game'' of
                Nothing -> do
                    sendLn csock $ B.pack "Game not found"
                    return ()
                Just game' -> do
                    if isAllConnected game'
                        then do
                            sendLn csock $ B.pack "full"
                            close csock
                        else do

                            let (game, plID) = joinGame (Client csock ip) game'

                            sendLn csock $ B.pack $ show plID

                            update' games game

                            putStrLn $ show game

                            clientLoop games gameId' (Client csock ip) (WaitingForPlayers 0)
