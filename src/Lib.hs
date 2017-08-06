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

    send csock
        $ B.append 
            (B.pack "Games:") 
            $ B.intercalate (B.pack ",") $ map (B.pack . show . getId) gamesInProgress

    gameId <- readMaybe <$> B.unpack <$> recv csock 100
    
    case gameId of
        Nothing -> do
            send csock $ B.pack "gameId must be an int!"
            close csock
            return ()
        Just gameId' -> do
            game <-
                joinGame (Client csock ip) <$>
                getGameMake' games gameId'
            
            update' games game

            putStrLn $ show game

            clientLoop games gameId' (Client csock ip) (WaitingForPlayers 0)

