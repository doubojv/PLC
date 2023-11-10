vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 12
vendas 3 = 18
vendas 4 = 20

totalVendas :: Int -> Int 
totalVendas n | n == 0 = vendas 0
              | otherwise = totalVendas(n - 1) + vendas n

mediaVendas :: Int -> Float
mediaVendas n = fromIntegral(totalVendas n) / fromIntegral(n + 1)

imprimirSequenciaCrescente :: Int -> IO ()
imprimirSequenciaCrescente n = go 0
  where
    go atual
      | atual > n = return ()  -- Caso base: quando chegamos a n+1, terminamos a recursão.
      | otherwise = do
          putStrLn (show atual)
          go (atual + 1)
          
{- imprimeTabela :: Int -> IO()
imprimeTabela n = putStr("Semana     Venda "
 ++ imprimeDecrescente n ++  
 show(totalVendas n) ++ 
 show(mediaVendas n)) -}


main = do
    putStr("")
