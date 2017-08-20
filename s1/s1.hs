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

import Data.ByteString.Lazy (cycle, fromStrict)

--c3
import Data.ByteString (mapAccumL, singleton)
import Data.Char (chr, isLetter, toLower)
import Data.List (sortBy)
import Data.Word (Word8)

import Data.Function (on)

--c4
--no new libraries for c4

--c5
import Data.ByteString.Char8 (unpack)

--c6
import Data.ByteString.Base16 (encode)
import Data.Bits (popCount)
import Data.Char (digitToInt)

import qualified Data.ByteString.Base64 as B64 (decode)
import qualified Data.ByteString as BS (take, drop)

import Control.Monad (liftM2)

--c1
fromHex :: String -> ByteString
fromHex = fst . decode . pack

--c2
infixl 8 .:
(.:) = (.) . (.)

infixl 7 .-
(.-) = (flip .) . (.)

xor :: ByteString -> ByteString -> ByteString
xor = BS.pack .: DBL.zipWith Bits.xor
  .- cycle . fromStrict
  .- fromStrict
{-
xor :: ByteString -> ByteString -> ByteString
xor s1 s2 = BS.pack $ DBL.zipWith Bits.xor s1' s2'
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
xor1 = xor . singleton . fromIntegral

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

--c5
repKeyXOR :: ByteString -> ByteString -> ByteString
repKeyXOR = xor

repKeyXOR' :: String -> String -> String
repKeyXOR' = unpack .: repKeyXOR .- pack .- pack

--c6
hammingdist :: Integral a => ByteString -> ByteString -> a
hammingdist = sum
  . map (fromIntegral . popCount . digitToInt)
  . unpack
  . encode
  .: xor

hammingdist' :: Integral a => String -> String -> a
hammingdist' = hammingdist .- pack .- pack

b64str2bs :: String -> Either String ByteString
b64str2bs = B64.decode . pack


sample :: Integral a => Int -> a -> ByteString -> [ByteString]
sample size nsamples = helper nsamples []
  where
    helper :: Integral a => a -> [ByteString] -> ByteString -> [ByteString]
    helper 0          acc text = acc
    helper remSamples acc text = helper (remSamples - 1)
                                        (BS.take size text : acc)
                                        (BS.drop size text)

infixl 8 .::
(.::) = (.) . (.:)
-- (.) . (.) . (.)

xorblocks :: Integral a => Int -> a -> ByteString -> [ByteString]
xorblocks =  map (uncurry xor) . (\l -> [ (a, b) | a <- l, b <- l, a < b ]) .:: sample

blockHD :: (Integral a, Floating b) => Int -> a -> ByteString -> b
blockHD size = mean . map normHD . getPairs .: (sample size)
  where
    size' :: Floating b => b
    size' = fromIntegral size

    normHD :: Floating b => (ByteString, ByteString) -> b
    normHD = (/size') . fromIntegral . uncurry hammingdist

    getPairs :: Ord c => [c] -> [(c,c)]
    getPairs list = [ (a, b) | a <- list, b <- list, a < b ]

    mean :: Floating b => [b] -> b
    mean = liftM2 (/) sum (fromIntegral . length)
