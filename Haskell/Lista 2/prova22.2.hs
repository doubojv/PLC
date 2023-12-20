import Data.Char (isDigit, digitToInt)


{- 1 QUESTÃO ---------------------------------------------}
rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (x:xs) | x /= 0 = (x: rlencode0 xs)
                 | otherwise = [0] ++ [countZeros (x:xs)] ++ rlencode0 (drop(countZeros xs) xs)

countZeros :: [Int] -> Int               
countZeros [] = 0
countZeros (x:xs) | x == 0 = 1 + countZeros xs
                  | otherwise = 0
             
rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 (0:xs) = showZeros (head xs) ++ rldecode0 (tail xs)
rldecode0 (x:xs) = [x] ++ rldecode0 xs


showZeros :: Int -> [Int]
showZeros 0 = []
showZeros n = [0] ++ showZeros (n - 1)


{- 2 QUESTÃO ----------------------------------------------}

rlencodeLetras :: String -> String
rlencodeLetras "" = ""
rlencodeLetras (x :"") = [x]
rlencodeLetras (x:xs) | x /= (head xs) = (x: rlencodeLetras xs)
                      | otherwise = [x] ++ show (countLetters (takeWhile (==x) (x:xs))) ++ rlencodeLetras (drop(countLetters (takeWhile (==x) xs)) xs)


countLetters :: String -> Int
countLetters "" = 1
countLetters (x:[]) = countLetters []
countLetters (x:xs) | x == (head xs) = 1 + countLetters xs
                    | otherwise = countLetters xs



rldecodeLetras :: String -> String
rldecodeLetras "" = ""
rldecodeLetras (x : "") = [x]
rldecodeLetras (x:xs) | not(isDigit (head xs)) = (x: rldecodeLetras xs) 
                      | otherwise = showLetters [x] (digitToInt(head xs)) ++ rldecodeLetras (tail xs)


showLetters :: String -> Int -> String
showLetters "" n = ""
showLetters letter 0 = ""
showLetters letter n = letter ++ showLetters letter (n -1)  


{- 3 QUESTÃO -------------------------------------------------}

data Letra = Unica Char| Repetida Char Int
                        deriving Show

rlencodeLetrasCodigo :: String -> [Letra]
rlencodeLetrasCodigo "" = []
rlencodeLetrasCodigo (x:"") = [Unica x]
rlencodeLetrasCodigo (x:xs) | x /= (head xs) = [Unica x] ++ rlencodeLetrasCodigo xs
                            | otherwise = [Repetida x (countLetters (takeWhile (==x) (x:xs)))] ++ rlencodeLetrasCodigo (drop(countLetters (takeWhile (==x) xs)) xs)


rldecodeLetrasCodigo :: [Letra] -> String
rldecodeLetrasCodigo [] = ""
rldecodeLetrasCodigo (Unica x : xs) = [x] ++ rldecodeLetrasCodigo xs 
rldecodeLetrasCodigo (Repetida x z : xs) = showLetters [x] z ++ rldecodeLetrasCodigo xs










