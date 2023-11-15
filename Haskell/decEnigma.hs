decEnigma :: String -> [(Char, Char)] -> String
decEnigma word [] = ""

decEnigma (x:xs) vector@((v, c) : t) | x == v = function (x:xs) vector
                                     | otherwise = decEnigma (x:xs) (t ++ [(v,c)])
                                      where
                                     function [] _ = ""
                                     function (x:xs) ((v, c) : t) | x == v = c : function xs vector
                                                                  | otherwise = function (x:xs) t
                

{-

jub
[('b','b'), ('u','o'), ('j','d')]

Output

"dob" -}

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result