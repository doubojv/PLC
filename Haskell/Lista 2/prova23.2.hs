-- jvcn Prova 1 PLC

------------------------ 1 Questao

fib :: Int -> [Int]
fib 2 = [0]
fib 3 = [0,1]
fib n =  fib (n - 1) ++ [last (fib (n - 1)) + last (fib (n - 2))]  -- Gera a sequencia de fibonacci ate n elemento da lista

fibonacci :: [Int]
fibonacci = concatMap fib [1..]

----------------------- 2 Questao

merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] lista2 = lista2
merge lista1 [] = lista1
merge (x:xs)(y:ys) | x > y = [y] ++ merge (x:xs) ys
                   | otherwise = [x] ++ (merge xs (y:ys))


----------------------- 3 Questao

mergesort :: Ord t => [t] -> [t]
mergesort [] = []
mergesort (x:xs)  | xs /= [] =  (merge (merge [x] [head xs]))  (mergesort (tail xs))
                  | otherwise = mergesort [] ++ [x]
 


--------------------- 4 Questao
type Pilha t = [t]

data Elemento = Valor Int | Soma | Multiplica
                          deriving(Show)


gera_string :: Pilha Elemento -> String
gera_string [] = ""
gera_string (Valor n : Soma : xs) = "+" ++ show n ++ gera_string xs ++ ")"
gera_string (Valor n : Multiplica : xs) = "*" ++ show n ++ gera_string xs ++ ")"
gera_string (Valor n : Valor m : Soma : xs) = "(" ++ show n  ++ "+" ++ show m ++ ")" ++ gera_string xs
gera_string (Valor n : Valor m : Multiplica : xs) = "(" ++ show n  ++ "*" ++ show m ++ ")" ++ gera_string xs



-------------------- 5 Questao

calcula :: Pilha Elemento -> Int
calcula l = calculadora (reverse l)

calculadora :: Pilha Elemento -> Int
calculadora [] = 0
calculadora (Soma : Valor n : Valor m : xs) = n + m
calculadora (Multiplica : Valor n : Valor m : xs) = n * m 
calculadora (Multiplica : Valor n : xs) = n * calculadora xs 
calculadora (Soma : Valor n : xs) = n + calculadora xs

