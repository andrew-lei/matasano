import S1 (solveRepXOR)

import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64 (decode)

main = do
  file <- readFile "6.txt"
  either (putStrLn) (putStrLn . unpack) $ solveRepXOR 2 40 10 <$> (decode . pack . filter (/='\n')) file
