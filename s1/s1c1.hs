import S1 (fromHex)

import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (unpack)

main = readFile "1.txt" >>= putStrLn . unpack . encode . fromHex
