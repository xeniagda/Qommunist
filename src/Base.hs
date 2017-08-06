
module Base where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

setAt :: [a] -> Int -> a -> [a]
setAt (a:b) 0 elem = elem : b
setAt (a:b) n elem = a : setAt b (n-1) elem

data Client = Client Socket SockAddr
    deriving (Show, Eq)
