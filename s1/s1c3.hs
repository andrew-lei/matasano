import S1 (highscore, toHex, xor256)

import Data.ByteString.Char8 (unpack)

main = readFile "3.txt"
  >>= putStrLn
  <$> unpack
  <$> highscore
  <$> xor256
  <$> toHex