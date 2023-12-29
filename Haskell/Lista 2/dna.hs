data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

dna1 :: Tree Int -> [String]
dna1 t = treeList (aux t) "" 0

conversor :: Int -> Char
conversor x  | mod x 5 == 0 = 'E'
             | mod x 5 == 1 = 'M'
             | mod x 5 == 2 = 'A'
             | mod x 5 == 3 = 'C'
             | otherwise = 'S'

aux :: Tree Int -> String
aux Nilt = ""
aux (Node a al ar) =  aux al ++ [conversor a] ++ aux ar

treeList :: String -> String -> Int -> [String]
treeList "" "" n = []
treeList "" a n = [a]
treeList (a:as) b n | mod n 8 == 7 = (b++[a]) : treeList as "" 0
                    | otherwise = treeList as (b++[a]) (n+1)


main = do
    bin <- getLine
    let result = dna1 (read bin::Tree Int)
    print result