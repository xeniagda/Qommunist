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

pingLimit = 10

-- Standard vector direction is:
--  +x -> Right
--  -x -> Left
--  +y -> Up
--  -y -> Down

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


data Player a
    = PlayerPawn (Vec2 a) Edge
    | Government
        Integer     -- Walls left
    deriving (Show, Eq)

data NetPlayer a
    = NetPlayer a Client
    | Waiting a
    deriving (Show, Eq)

-- extract unwraps a player,
-- You can use (<$ player)
-- to put it back in context

extract (NetPlayer a _) = a
extract (Waiting a) = a

instance Functor NetPlayer where
    fmap f (NetPlayer x cl) = NetPlayer (f x) cl
    fmap f (Waiting x) = Waiting (f x)

instance Applicative NetPlayer where
    pure a = Waiting a
    Waiting f <*> Waiting x = Waiting $ f x
    NetPlayer f cl <*> Waiting x = NetPlayer (f x) cl
    Waiting f <*> NetPlayer x cl = NetPlayer (f x) cl
    NetPlayer f cl <*> NetPlayer x _ = NetPlayer (f x) cl

data Winner
    = NoWin
    | GovWin
    | PawnWin

data Game =
    Game
        { getSize :: Integer
        , getWalls :: [Wall Integer]
        , getPlayers :: [NetPlayer (Player Integer)]
        , getPlayerTurn :: Int
        , getId :: Int
        }
    deriving (Show, Eq)

getWinningPlayers game =
    filter (\pawn -> case extract pawn of
        PlayerPawn (Vec2 x y) edge ->
            case edge of
                Right -> x == getSize game - 1
                Left -> x == 0
                Up -> y == getSize game - 1
                Down -> y == 0
        _ -> False
    ) $ getPlayers game

getWinner :: Game -> Winner
getWinner game =
    let gov = getGov game
    in case extract gov of
            Government 0 -> PawnWin
            _ -> case getWinningPlayers game /= [] of
                True -> PawnWin
                False -> NoWin

getGov :: Game -> NetPlayer (Player Integer)
getGov game =
    let govs = filter 
            (\x -> case extract x of
                Government _ -> True
                _ -> False
            ) $ getPlayers game
    in head govs

data Move
    = PawnMove Int (Vec2 Integer)
    | PlaceWall (Wall Integer)
    deriving (Show, Eq)

parseMove :: Int -> Game -> B.ByteString -> Maybe Move
parseMove playerIdx game x =
    let player = extract $ getPlayers game !! playerIdx
    in case B.uncons x of
        Nothing -> Nothing
        Just (x, rest) ->
            case x of
                'p' -> -- Go
                    case player of
                        PlayerPawn _ _ -> Nothing
                        _ -> parsePlaceWall rest
                'g' -> -- Go
                    case player of
                        Government _ -> Nothing
                        _ ->
                            let dir = 
                                    case B.head rest of
                                        'u' -> Just $ Vec2 0 1
                                        'd' -> Just $ Vec2 0 (-1)
                                        'l' -> Just $ Vec2 (-1) 0
                                        'r' -> Just $ Vec2 1 0
                                        _ -> Nothing
                            in case dir of
                                Just x -> Just $ PawnMove playerIdx x
                                Nothing -> Nothing
                _ -> Nothing

parsePlaceWall :: B.ByteString -> Maybe Move
parsePlaceWall cmd = do
    (x, restX) <- B.readInt cmd
    (d, restD) <- B.uncons restX
    let dir' = case d of
            'u' -> Just UpDown
            'r' -> Just RightLeft
            _ -> Nothing
    dir <- dir'
    (y, _) <- B.readInt restD
    return $ PlaceWall $ Wall (Vec2 (fromIntegral x) (fromIntegral y)) dir


canPlaceWall :: Wall Integer -> Game -> Bool
canPlaceWall (Wall (Vec2 x y) RightLeft) game =
    let intersectingWalls =
            filter (\ (Wall (Vec2 wx wy) dir) ->
                case dir of
                    UpDown ->
                        wx == x && wy == y
                    RightLeft ->
                        wy == y
                        &&
                        wx >= x - 1 && wx <= x + 1
            ) $ getWalls game
    in intersectingWalls == []
        && x > 0 && y > 0
        && x <= getSize game && x < getSize game

canPlaceWall (Wall (Vec2 x y) UpDown) game =
    let intersectingWalls =
            filter (\ (Wall (Vec2 wx wy) dir) ->
                case dir of
                    UpDown ->
                        wx == x
                        &&
                        wy >= y - 1 && wy <= y + 1
                    RightLeft ->
                        wx == x && wy == y
            ) $ getWalls game
    in intersectingWalls == []
        && x > 0 && y > 0
        && x <= getSize game && x < getSize game

canMove :: Vec2 Integer -> Vec2 Integer -> Game -> Bool
canMove pos@(Vec2 x y) vel@(Vec2 dx dy) game =
    let intersecting = getIntersectingWalls pos vel game
        inBounds =
                x + dx >= 0 && y + dy >= 0
            &&  x + dx <= getSize game && y + dy <= getSize game
    in inBounds && length intersecting == 0

getIntersectingWalls :: Vec2 Integer -> Vec2 Integer -> Game -> [Wall Integer]
getIntersectingWalls pos@(Vec2 x y) vel@(Vec2 dx dy) game =
    filter (\ (Wall (Vec2 wx wy) dir) ->
        case dir of
            UpDown ->
                (x < wx) /= (x + dx < wx)
                &&
                wy - 1 <= y && wy + 1 > y
            RightLeft ->
                (y < wy) /= (y + dy < wy)
                &&
                wx - 1 <= x && wx + 1 > x
    ) $ getWalls game

doMove :: Move -> Game -> (Game, Bool)
doMove (PlaceWall wall) game =
    let canPlace = canPlaceWall wall game
        game' =
            game {
                getWalls = wall : getWalls game,
                getPlayers =
                    map (\player ->
                        (<$ player) $
                        case extract player of
                            Government x -> Government $ x - 1
                            PlayerPawn x y -> PlayerPawn x y
                    )
                    $ getPlayers game
            }
    in 
        if canPlace
            then (game', True)
            else (game, False)

doMove (PawnMove plIdx (Vec2 dx dy)) game =
    --if plIdx >= length (getPlayers game)
    --    then (game, False)
    --    else
    let player = getPlayers game !! plIdx
        move pl =
            case pl of
                PlayerPawn (Vec2 x y) e ->
                    if canMove (Vec2 x y) (Vec2 dx dy) game
                        then Just $ PlayerPawn (Vec2 (x + dx) (y + dy)) e
                        else Nothing
                _ -> Nothing
        pawn = extract $ fmap move player
        (player', worked) =
            case pawn of
                Just x -> (x <$ player, True)
                Nothing -> (player, False)
        game' =
            game
                {
                    getPlayers =
                        setAt 
                            (getPlayers game)
                            plIdx
                            player'
                }
    in (game', worked)

makeGame size id =
    Game
        { getWalls = [ ]
        , getSize = size
        , getPlayers =
            [ Waiting $ PlayerPawn
                (Vec2 (size `quot` 2) 0)
                Up
            , Waiting $ PlayerPawn
                (Vec2 (size `quot` 2) size)
                Down
            -- , Waiting $ PlayerPawn
            --     (Vec2 0 (size `quot` 2))
            --     Right
            -- , Waiting $ PlayerPawn
            --     (Vec2 size (size `quot` 2))
            --     Left
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

joinGame :: Client -> Game -> (Game, Int)
joinGame client game =
    let firstWaitingIdx' =
            findIndex (\player ->
                case player of
                    Waiting _ -> True
                    _ -> False
            ) $ getPlayers game
    in case firstWaitingIdx' of
        Nothing -> (game, -1)
        Just firstWaitingIdx ->
            let Waiting pl = (getPlayers game) !! firstWaitingIdx
                game' = game
                    {
                        getPlayers =
                            setAt
                                (getPlayers game)
                                firstWaitingIdx
                                (NetPlayer pl client)
                    }
            in (game', firstWaitingIdx)

getGame :: TVar [Game] -> Int -> STM (Maybe Game) -- Get a game from the TVar.
getGame var id = do
    games <- readTVar var
    case filter ((== id) . getId) games of
        a:_ -> return $ Just a
        [] -> return Nothing

getGame' var id = atomically $ getGame var id

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

getNextId :: [Game] -> Int
getNextId games =
    let check n =
            if any ((==n) . getId) games
                then check $ n + 1
                else n
    in check 1

checkPings :: TVar [Game] -> Game -> Int -> Int -> IO Bool
checkPings games game pings playerId = do
    if pings > pingLimit
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
