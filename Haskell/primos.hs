ePrimo :: Integer -> Bool
ePrimo n
  | n <= 1  = False 
  | n == 2  = True  
  | otherwise = ePrimoAux n 2

ePrimoAux :: Integer -> Integer -> Bool
ePrimoAux n divisor
  | divisor * divisor > n = True 
  | n `mod` divisor == 0 = False 
  | otherwise = ePrimoAux n (divisor + 1)


main = do
    putStr("")
