import S1 (fromHex, nBlocks, same)

import Data.ByteString.Base16 (encode)
import qualified Data.ByteString as BS (concat)
import Data.ByteString.Char8 (unpack)

main = do
  text <- lines <$> readFile "8.txt"

  let text' = map fromHex text
  putStrLn
    . unpack
    . encode
    . BS.concat
    . concat
    . filter same
    . map (nBlocks 16)
    $ text'
