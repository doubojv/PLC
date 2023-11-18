sumList :: [Int] -> Int
sumList as | as == [] = 0
           | otherwise = (head as) + sumList (tail as)

somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos (x:t) 0 = ( 0 : somarMultiplos t 0)
somarMultiplos [] value = []
somarMultiplos (x:t) value | value <= x =  verify x value 0 ++  somarMultiplos t value
                           | otherwise = 0 : somarMultiplos t value
                                          where
                                          verify 0 value total = [total]
                                          verify x value total | x  `mod` value == 0 = verify (x - 1) value (total + x)
                                                               |otherwise = verify (x - 1) value total
                           
                                                                            
main = do                                   
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result