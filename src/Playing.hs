module Playing (clientLoop, GameState(..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Base
import Game
import EncodeGame

clientLoop :: TVar [Game] -> Int -> Client -> GameState -> IO ()
clientLoop games gameId client@(Client csock ip) (WaitingForPlayers n) = do
    game <- fromJust <$> getGame' games gameId

    if getNumOfConnected game /= n
        then do 
            sendLn csock $ B.append (encodeGame game) $ B.pack "\n"
            return ()
        else return ()

    if isAllConnected game
        then clientLoop games gameId client (Playing game)
        else do
            threadDelay (second `quot` 10)
            clientLoop games gameId client (WaitingForPlayers $ getNumOfConnected game)

clientLoop games gameId client@(Client csock ip) (Playing lastGame) = do

    game <- fromJust <$> getGame' games gameId

    threadDelay (second `quot` 10)

    if game /= lastGame
        then sendLn csock $ encodeGame game
        else return 0

    let playerId' =
            findIndex (\pl ->
                case pl of
                    NetPlayer _ cl -> cl == client
                    _ -> False
            ) $ getPlayers game
        playerId =
            case playerId' of 
                Just n -> n
                _ -> -1
        playing = playerId == getPlayerTurn game

    if playerId == -1
        then do
            putStrLn "Something went very wrong. PlayerId == -1"
            sendLn csock $ B.pack "EXIT something went wrong"
        else return 0

    -- Check if it's the player's turn
    
    if not playing
        then clientLoop games gameId client (Playing game)
        else return ()
        -- Sluta vara dum ghc tack jag blir sur på dig när du gör så här

    playerMove <- (parseMove playerId) <$> recv csock 100
    
    let game' =
            case playerMove of
                Just move -> 
                    let (moved, worked) = doMove move game
                    in
                        if worked 
                            then moved { getPlayerTurn = (getPlayerTurn moved + 1) `mod` length (getPlayers moved) }
                            else game
                Nothing -> game
            
    putStrLn $ show ip ++ " player " ++ show playerId ++ " did " ++ show playerMove

    update' games game'
    
    clientLoop games gameId client (Playing game)

data GameState
    = WaitingForPlayers Int
    | Playing Game -- Last game

