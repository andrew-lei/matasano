import S1 (fromHex, xor)

import Data.ByteString.Char8 (unpack)

main = do
  x <- fromHex <$> readFile "2a.txt"
  y <- fromHex <$> readFile "2b.txt"
  putStrLn . unpack $ x `xor` y
