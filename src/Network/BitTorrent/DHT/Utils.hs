module Network.BitTorrent.DHT.Utils where

import Data.Word
import Prelude as P
import Data.Bits

if' c a b = if c then a else b

toWord32 :: [Word8] -> Word32
toWord32 = P.foldr (\o a -> (a `shiftL` 8) .|. fromIntegral o) 0