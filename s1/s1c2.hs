import S1 (toHex, xor)

import Data.ByteString.Char8 (unpack)

main = do
  x <- toHex <$> readFile "2a.txt"
  y <- toHex <$> readFile "2b.txt"
  putStrLn . unpack $ x `xor` y
