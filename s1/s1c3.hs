import S1 (highscore, fromHex, xor256)

import Data.ByteString.Char8 (unpack)

main = readFile "3.txt"
  >>= putStrLn
  <$> unpack
  <$> highscore
  <$> xor256
  <$> fromHex
