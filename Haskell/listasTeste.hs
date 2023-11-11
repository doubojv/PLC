sumList :: [Int] -> Int
sumList as | as == [] = 0
           | otherwise = (head as) + sumList (tail as)

doubo :: [Int] -> [Int]
doubo [] = [] -- Se a lista estiver vazia, retorna uma lista vazia
doubo (x:xs) = (x * 2) : doubo xs -- Multiplica o primeiro elemento por 2 e continua com o restante da lista
           
main = do
    putStr("")