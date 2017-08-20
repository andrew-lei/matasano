import S1 (fromHex, xor)

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Base16 (encode)

main = do
  x <- fromHex <$> readFile "2a.txt"
  y <- fromHex <$> readFile "2b.txt"
  putStrLn . unpack . encode $ x `xor` y
