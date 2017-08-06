module Game where

import Prelude hiding (Left, Right)
import Data.List

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Data.ByteString.Char8 as B
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Vec
import Base

-- Standard vector direction is:
--  +x -> Right
--  -x -> Left
--  +y -> Up
--  -y -> Down

second :: Int
second = 1000 * 1000
defaultSize = 8

data Wall a =
    Wall (Vec2 a) WallDirection
    deriving (Show, Eq)

data WallDirection
    = RightLeft
    | UpDown
    deriving (Show, Eq)

data Edge
    = Up
    | Down
    | Left
    | Right
    deriving (Show, Eq)

data Color
    = White
    | Yellow
    | Brown
    | Black
    deriving (Show, Eq)

data Pawn a
    = Pawn (Vec2 a) Edge Color
    deriving (Show, Eq)

data Player a
    = PlayerPawn (Pawn a)
    | Government
        Integer     -- Walls left
    deriving (Show, Eq)

data NetPlayer a
    = NetPlayer (Player a) Client
    | Waiting (Player a)
    deriving (Show, Eq)

data Game =
    Game
        { getSize :: Integer
        , getWalls :: [Wall Integer]
        , getPlayers :: [NetPlayer Integer]
        , getPlayerTurn :: Int
        , getId :: Int
        }
    deriving (Show, Eq)


makeGame size id =
    Game
        { getWalls = 
            [ Wall (Vec2 0 2) UpDown
            , Wall (Vec2 3 5) RightLeft
            ]
        , getSize = size
        , getPlayers =
            [ Waiting $ PlayerPawn $ Pawn
                (Vec2 (size `quot` 2) 0)
                Up
                White
            , Waiting $ PlayerPawn $ Pawn
                (Vec2 (size `quot` 2) size)
                Down
                Yellow
            , Waiting $ PlayerPawn $ Pawn
                (Vec2 0 (size `quot` 2))
                Right
                Brown
            , Waiting $ PlayerPawn $ Pawn
                (Vec2 size (size `quot` 2))
                Left
                Black
            , Waiting $ Government
                ((size * size) `quot` 4)
            ]
        , getPlayerTurn = 0
        , getId = id
        }

isAllConnected :: Game -> Bool
isAllConnected game =
    all
        (\player ->
            case player of
                NetPlayer _ _ -> True
                Waiting _ -> False
        )
        $ getPlayers game

getNumOfConnected :: Game -> Int
getNumOfConnected game =
    length $ filter
        (\player ->
            case player of
                NetPlayer _ _ -> True
                Waiting _ -> False
        )
        $ getPlayers game

joinGame :: Client -> Game -> Game
joinGame client game =
    let firstWaitingIdx' =
            findIndex (\player ->
                case player of
                    Waiting _ -> True
                    _ -> False
            ) $ getPlayers game
    in case firstWaitingIdx' of
        Nothing -> game
        Just firstWaitingIdx ->
            let Waiting pl = (getPlayers game) !! firstWaitingIdx
            in game {
                getPlayers = 
                    setAt
                        (getPlayers game)
                        firstWaitingIdx
                        (NetPlayer pl client)
            }

getGame :: TVar [Game] -> Int -> STM (Maybe Game) -- Get a game from the TVar.
getGame var id = do
    games <- readTVar var
    case filter ((== id) . getId) games of
        a:_ -> return $ Just a
        [] -> return Nothing

getGame' var id = atomically $ getGame var id

getGameMake :: TVar [Game] -> Int -> STM Game -- Get a game from the TVar. If it doesn't exist, make it
getGameMake var id = do
    game <- getGame var id
    case game of
        Just game -> return game
        Nothing -> do
                let game = makeGame defaultSize id
                modifyTVar var (game:)
                return game

getGameMake' var id = atomically $ getGameMake var id

update :: TVar [Game] -> Game -> STM ()
update tgames game =
    modifyTVar tgames (
        map (\g -> 
                if getId g == getId game
                    then game
                    else g
            )
    )

update' tgames game = atomically $ update tgames game
