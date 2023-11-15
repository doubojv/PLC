ePrimo :: Int -> Bool
ePrimo n
  | n <= 1  = False 
  | n == 2  = True  
  | otherwise = ePrimoAux n 2

ePrimoAux :: Int -> Int -> Bool
ePrimoAux n divisor
  | divisor * divisor > n = True 
  | n `mod` divisor == 0 = False 
  | otherwise = ePrimoAux n (divisor + 1)


fatPrime :: Int -> [(Int, Int)]
fatPrime number | ePrimo number = [(number, 1)]
                | otherwise = factors number 2 0
                where 
                    factors number aux index | number `mod` aux  == 0 = counter (number `div` aux) aux index
                                             | (number < aux) && (index == 0) = []
                                             | index /= 0 =  [(aux,index)] ++ factors number (aux + 1) 0
                                             | otherwise = factors number (aux + 1) 0
                                                where 
                                                counter div aux total | div /= 1 = factors div aux (index + 1)
                                                                      | otherwise = factors div aux (index + 1)
                                                                 

{- 

20  | 2
10  | 2
5   | 5
1
= [(2,2), (5,1)]
 -}


main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result