unzip1 :: [(a,b)] -> ([a],[b])
unzip1 [] = ([],[])
unzip1 ((a,b): xs) = (a: fst(unzip1 xs), b: snd(unzip1 xs))


main = interact $ show . unzip1 . (read :: String -> [(Int,Int)])
