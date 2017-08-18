import S1 (highscore, toHex)

import Data.ByteString.Char8 (unpack)

main = readFile "3.txt"
  >>= putStrLn
  <$> unpack
  <$> highscore
  <$> toHex
