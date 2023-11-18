minMaxCartao :: String -> (Double,Double)
minMaxCartao "" = (0.0,0.0)
minMaxCartao string = (menorElemento (extractPrices (toString string)), maiorElemento (extractPrices(toString string)))


dropVirg :: String -> String
dropVirg [] = []
dropVirg [';'] = []
dropVirg (';':xs) = xs
dropVirg (x:xs) = dropVirg xs

toString :: String -> [String]
toString "" = []
toString (h:t) | h /= ';' = [h : toString' t] ++ toString (dropVirg t)

toString' :: String -> String
toString' "" = ""
toString' (c:t) | c == ';' = ""
                | otherwise = c : toString' t

extractPrices :: [String] -> [Double]
extractPrices [] = []
extractPrices (_:_:price:rest) = stringParaDouble price : extractPrices rest

maiorElemento :: [Double] -> Double
maiorElemento lista = maximum lista

menorElemento :: [Double] -> Double
menorElemento lista = minimum lista

stringParaDouble :: String -> Double
stringParaDouble str = read str

main = do
    a <- getLine
    let result = minMaxCartao a
    print result