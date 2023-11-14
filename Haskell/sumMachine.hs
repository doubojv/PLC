maquinaSomar :: [Int] -> [Int]
maquinaSomar (x:xs) | 





{-  [0,0,1,2,3,0,1] -}




main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])