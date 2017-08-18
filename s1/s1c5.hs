import S1 (repKeyXOR')

main = do
  key <- readFile "5key.txt"
  message <- readFile "5message.txt"
  let encry = key `repKeyXOR'` message
  print encry
  putStr $ key `repKeyXOR'` encry
