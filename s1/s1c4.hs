import S1 (highscore, fromHex, xor256)

import Data.ByteString.Char8 (unpack)

main = readFile "4.txt"
  >>= putStr
  <$> unpack
  <$> highscore
  <$> map (highscore . xor256 . fromHex)
  <$> lines
