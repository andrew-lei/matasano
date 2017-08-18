import S1 (highscore, toHex, xor256)

import Data.ByteString.Char8 (unpack)

main = readFile "4.txt"
  >>= putStr
  <$> unpack
  <$> highscore
  <$> map (highscore . xor256 . toHex)
  <$> lines
