
addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | otherwise = addEspacos (n - 1) ++ " "
             

paraDireita :: Int -> String -> String
paraDireita n frase | n == 0 = frase
                    | otherwise = addEspacos n ++ frase


main = do
    putStr("")