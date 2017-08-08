
module Base where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

setAt :: [a] -> Int -> a -> [a]
setAt (a:b) 0 elem = elem : b
setAt (a:b) n elem = a : setAt b (n-1) elem

data Client = Client Socket SockAddr
    deriving (Show, Eq)

sendLn :: Socket -> B.ByteString -> IO Int
sendLn sock x = send sock $ B.snoc x '\n'
