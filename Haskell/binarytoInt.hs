charParaInt :: Char -> Int
charParaInt '0' = 0
charParaInt '1' = 1


btoi :: String -> Int
btoi "" = 0
btoi (b:bs) | charParaInt b == 1 = 2^length bs +  btoi bs
            | otherwise = btoi bs 


main = do
    s <- getLine
    let result = btoi s
    print result