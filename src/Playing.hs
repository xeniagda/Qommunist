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
clientLoop games gameId client@(Client ip tPings tRecv tSend) (WaitingForPlayers n) = do

    pings <- readTVarIO tPings

    game <- fromJust <$> getGame' games gameId

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

    putStrLn $ show ip ++ " has " ++ show pings ++ " pings"

    if playerId == -1
        then do
            putStrLn "Something went very wrong. PlayerId == -1"
            cSend client $ B.pack "EXIT something went wrong"
        else return ()

    checkPings games game pings playerId


    if getNumOfConnected game /= n
        then do 
            cSendLn client $ B.append (encodeGame game) $ B.pack "\n"
            return ()
        else return ()

    if isAllConnected game
        then clientLoop games gameId client (Playing game)
        else do
            threadDelay (second `quot` 10)
            clientLoop games gameId client (WaitingForPlayers $ getNumOfConnected game)

clientLoop games gameId client@(Client ip tPings tRecv tSend) (Playing lastGame) = do

    pings <- readTVarIO tPings


    game <- fromJust <$> getGame' games gameId

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
            cSend client $ B.pack "EXIT something went wrong"
        else return ()

    checkPings games game pings playerId

    putStrLn $ show ip ++ " has " ++ show pings ++ " pings"

    threadDelay (second `quot` 10)

    if game /= lastGame
        then cSendLn client $ encodeGame game
        else return ()

    if not $ isAllConnected game
        then clientLoop games gameId client $ WaitingForPlayers (-1)
        else do

            -- Check if it's the player's turn
            
            if not playing
                then clientLoop games gameId client (Playing game)
                else return ()
                -- Sluta vara dum ghc tack jag blir sur på dig när du gör så här

            playerMove <- (parseMove playerId game) <$> cRecvWait client
            
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

checkPings games game pings playerId = do
    if pings > 10
        then do -- Disconnect player
            let gameWithoutPlayer =
                    game {
                        getPlayers =
                            setAt
                                (getPlayers game)
                                playerId
                                (Waiting $ extract (getPlayers game !! playerId))
                    }
            update' games gameWithoutPlayer
            return False
        else
            return True

data GameState
    = WaitingForPlayers Int
    | Playing Game -- Last game
