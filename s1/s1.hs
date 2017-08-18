module S1 where

--c1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack)

toHex :: String -> ByteString
toHex = fst . decode . pack
