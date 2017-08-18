module S1 where
import Prelude hiding (cycle)

--c1
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Char8 (pack)

--c2
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString as BS (pack)
import qualified Data.ByteString.Lazy as DBL (zipWith)

import Data.ByteString.Base16 (encode)
import Data.ByteString.Lazy (cycle, fromStrict)

import Data.Aviary.Birds (cardinal')

--c3
import Data.ByteString (mapAccumL, singleton)
import Data.Char (chr, isLetter, toLower)
import Data.List (sortBy)
import Data.Word (Word8)

import Data.Function (on)

--c4
--no new libraries for c4

--c1
toHex :: String -> ByteString
toHex = fst . decode . pack

--c2
infixl 8 .:
(.:) = (.) . (.)

infixl 7 .-
(.-) = cardinal'

xor :: ByteString -> ByteString -> ByteString
xor = encode . BS.pack .: DBL.zipWith Bits.xor
  .- cycle . fromStrict
  .- fromStrict
{-
xor :: ByteString -> ByteString -> ByteString
xor s1 s2 = encode . BS.pack $ DBL.zipWith Bits.xor s1' s2'
  where
    s1' = cycle . fromStrict $ s1
    s2' = fromStrict s2
-}

--c3
scorer :: Char -> Int
scorer ' '     = 14
scorer 'e'     = 13
scorer 't'     = 12
scorer 'a'     = 11
scorer 'i'     = 10
scorer 'o'     = 9
scorer 'n'     = 8
scorer 's'     = 7
scorer 'h'     = 6
scorer 'r'     = 5
scorer 'd'     = 4
scorer 'l'     = 3
scorer 'u'     = 2
scorer c
  | isLetter c = 1
  | otherwise  = 0

w8scorer :: Word8 -> Int
w8scorer = scorer . chr . fromIntegral

xor1 :: Int -> ByteString -> ByteString
xor1 = fst . decode .: xor . singleton . fromIntegral

xor256 :: ByteString -> [ByteString]
xor256 = flip map [0..255] . flip xor1
--xor256 s = map (flip xor1 s) [0..255]

score :: ByteString -> (Int, ByteString)
score = mapAccumL (\acc w -> (acc + w8scorer w, w)) 0

scoreAndSort :: [ByteString] -> [(Int, ByteString)]
scoreAndSort = sortBy likelier . map score
  where
    likelier = flip compare `on` fst

highscore :: [ByteString] -> ByteString
highscore = snd . head . scoreAndSort

--c4
--no new functions for c4
