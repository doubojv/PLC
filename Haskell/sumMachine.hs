maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []  
maquinaSomar lista | somaAteZero lista /= 0 || check lista = somaAteZero lista : maquinaSomar (dropZeros lista)
                   | otherwise = maquinaSomar (dropZeros lista)

somaAteZero :: [Int] -> Int
somaAteZero [] = 0
somaAteZero (0:_) = 0
somaAteZero (x:xs) = x + somaAteZero xs

check :: [Int] -> Bool
check (x:[]) = False
check (x:y:t) | (x + y == 0) && (x /= 0 && y /= 0) = True
              | otherwise = False


dropZeros :: [Int] -> [Int]
dropZeros [] = []
dropZeros [0] = []
dropZeros (0:xs) = function xs
                   where 
                    function (b:bs) | b /= 0 = b:bs
                                    | otherwise = []

dropZeros (x:xs) | length (x:xs) /= 1 =  dropZeros xs
                 | otherwise = []

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])