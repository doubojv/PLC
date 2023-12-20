fibonacci :: Int -> [Int]
fibonacci 1 = [0]
fibonacci 2 = [0,1]
fibonacci n =  fibonacci (n - 1) ++ [last (fibonacci (n - 1)) + last (fibonacci (n - 2))] 


somarPares :: [Int] -> Int
somarPares [] = 0
somarPares (x:xs) | x `mod` 2 == 0 = x + somarPares xs 
                  | otherwise = somarPares xs

pares :: String -> String 
pares "" = ""
pares word = word ++ show (somarPares (fibonacci (length word))) 

main = do
    input <- getLine 
    let result = pares input
    print result