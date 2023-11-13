sumNumbers :: String -> Int
sumNumbers str = go str 0 0
  where
    go [] total current = total + current
    go (c:cs) total current
      | c >= '0' && c <= '9' =
          let digit = fromEnum c - fromEnum '0'
          in if current /= 0
              then go cs (total + current) digit
              else go cs total digit
      | current /= 0 = go cs (total + current) 0
      | otherwise = go cs total 0

main = do
  a <- getLine
  let result = sumNumbers a
  print result