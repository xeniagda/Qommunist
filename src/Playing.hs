module Playing (clientLoop, GameState(..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Base
import Game
import EncodeGame

clientLoop :: TVar [Game] -> Int -> Client -> GameState -> IO ()
clientLoop games gameId client@(Client csock ip) (WaitingForPlayers n) = do
    game <- getGameMake' games gameId

    if getNumOfConnected game /= n
        then do 
            send csock $ B.append (encodeGame game) $ B.pack "\n"
            return ()
        else return ()

    if isAllConnected game
        then clientLoop games gameId client Playing
        else do
            threadDelay (second `quot` 10)
            clientLoop games gameId client (WaitingForPlayers $ getNumOfConnected game)

clientLoop games gameId client@(Client csock ip) Playing = do
    game <- getGameMake' games gameId

    send csock $ B.pack "All connected"
    close csock

data GameState
    = WaitingForPlayers Int
    | Playing
