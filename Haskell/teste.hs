vendas :: Int -> Int
vendas 0 = 30
vendas 1 = 30
vendas 2 = 40
vendas 3 = 40
vendas 4 = 50
 
vendasIguaisA :: Int -> Int -> Int
vendasIguaisA valor 0 | vendas 0 == valor = 1
                      | otherwise = 0
vendasIguaisA valor semana | vendas semana == valor = vendasIguaisA valor (semana - 1) + 1
                           | otherwise = vendasIguaisA valor (semana - 1)

main = do
    putStr("")
    
