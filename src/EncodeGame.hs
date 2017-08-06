module EncodeGame where

import Prelude hiding (Left, Right)
import qualified Data.ByteString.Char8 as B

import Game
import Base
import Vec

encodeGame :: Game -> B.ByteString
encodeGame game =
    B.intercalate (B.pack ";")
        [ B.pack $ show $ getSize game
        , encodeWalls $ getWalls game
        , encodePlayers $ getPlayers game
        , B.pack $ show $ getPlayerTurn game
        , B.pack $ show $ getId game
        ]

encodeWalls :: (Show a) => [Wall a] -> B.ByteString
encodeWalls walls =
    B.intercalate (B.pack ",") $ map encodeWall walls

encodeWall :: (Show a) => Wall a -> B.ByteString
encodeWall (Wall (Vec2 x y) dir) =
    B.intercalate (B.pack " ")
        [ B.pack $ show x
        , B.pack $ show y
        , B.pack $ case dir of
            RightLeft -> "r"
            UpDown -> "u"
        ]

encodePlayers :: (Show a) => [NetPlayer a] -> B.ByteString
encodePlayers players =
    B.intercalate (B.pack ",") $ map encodeNetPlayer players

encodeNetPlayer :: (Show a) => NetPlayer a -> B.ByteString
encodeNetPlayer (NetPlayer pl (Client _ ip)) =
    B.intercalate (B.pack " ")
        [ B.pack "n"
        , encodePlayer pl
        , B.pack $ show ip
        ]
encodeNetPlayer (Waiting pl) =
    B.append (B.pack "w ") (encodePlayer pl)


encodePlayer :: (Show a) => Player a -> B.ByteString
encodePlayer (PlayerPawn (Pawn (Vec2 x y) edge col)) =
    B.intercalate (B.pack " ")
        [ B.pack "p"
        , B.pack $ show x
        , B.pack $ show y
        , B.pack $ case edge of
            Up -> "u"
            Down -> "d"
            Left -> "l"
            Right -> "r"
        , B.pack $ case col of
            White -> "w"
            Yellow -> "y"
            Brown -> "b"
            Black -> "l"
        ]
encodePlayer (Government i) =
    B.intercalate (B.pack " ")
        [ B.pack "g"
        , B.pack $ show i
        ]
