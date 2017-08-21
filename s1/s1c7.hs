import Crypto.Cipher.Types (cipherInit, ecbDecrypt)
import Crypto.Cipher.AES (AES128)
import Crypto.Error (eitherCryptoError)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64 (decode)

import Data.Either.Combinators (mapLeft)

main = do
  keystr <- filter (/='\n') <$> readFile "7key.txt"
  b64message <- readFile "7.txt"

  let key :: Either String AES128
      key = mapLeft show . eitherCryptoError . cipherInit . pack $ keystr

  let message :: Either String ByteString
      message = decode . pack . concat . lines $ b64message

  either putStrLn (putStr . unpack) $ ecbDecrypt <$> key <*> message
