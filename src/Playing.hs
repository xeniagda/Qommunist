module Playing (clientLoop, GameState(..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Base
import Game
import EncodeGame

showDroppedPings = False

clientLoop :: TVar [Game] -> Int -> Client -> IO ()
clientLoop games gameId client@(Client ip tPings tRecv tSend) = do
    clientLoopState $ WaitingForPlayers (-1)
    where
        clientLoopState state = do
            threadDelay (second `quot` 10)

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
    
            pings <- readTVarIO tPings
            stillIn <- checkPings games game pings playerId

            when (pings > 5 && showDroppedPings) $ do
                putStrLn $ show client ++ " has " ++ show pings ++ " dropped pings"

            if stillIn 
                then do
                    let winner = getWinner game
                        state' = case winner of
                            NoWin -> state
                            _ -> Won game

                    newState <- handleStep games gameId client state'
                    case newState of
                        Just st -> clientLoopState st
                        Nothing -> return ()
                else do
                    putStrLn $ show client ++ " died of too many dropped pings"

handleStep :: TVar [Game] -> Int -> Client -> GameState -> IO (Maybe GameState)
handleStep games gameId client@(Client ip tPings tRecv tSend) state@(WaitingForPlayers n) = do

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


    if getNumOfConnected game /= n
        then do 
            cSendLn client $ B.append (encodeGame game) $ B.pack "\n"
            return ()
        else return ()

    if isAllConnected game
        then return $ Just $ Playing game
        else do
            return $ Just $ WaitingForPlayers $ getNumOfConnected game

handleStep games gameId client@(Client ip tPings tRecv tSend) (Playing lastGame) = do
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

    if game /= lastGame
        then cSendLn client $ encodeGame game
        else return ()

    if not $ isAllConnected game
        then return $ Just $ WaitingForPlayers (-1)
        else do

            -- Check if it's the player's turn
            
            if not playing
                then return $ Just $ Playing game
                else do -- Sluta vara dum ghc tack jag blir sur på dig när du gör så här

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
                    
                    return $ Just $ Playing game

handleStep games gameId client@(Client ip tPings tRecv tSend) state@(Won lastGame) = do
    game <- fromJust <$> getGame' games gameId
    if game /= lastGame
        then cSendLn client $ encodeGame game
        else return ()

    return $ Just $ Won game

data GameState
    = WaitingForPlayers Int
    | Playing Game -- Last game
    | Won Game
