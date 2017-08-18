module S1 where

--c1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack)

--c2
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString as BS (zipWith, pack)
import Data.ByteString.Base16 (encode)

--c1
toHex :: String -> ByteString
toHex = fst . decode . pack

--c2
--'blackbird' combinator
--compose a function of two arguments to a function of one
infixl 8 .:
(.:) = (.) . (.)

xor :: ByteString -> ByteString -> ByteString
xor = encode . BS.pack .: BS.zipWith Bits.xor
