import Data.Char (isDigit)
import Data.List (isInfixOf)

logMes :: String -> String -> Double
logMes [] string = 0.0
logMes x y = foldl(+) 0 (extractPrices(toString y) x)

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

extractPrices :: [String] -> String -> [Double]
extractPrices [] mes = []
extractPrices (x:_:price:rest) mes | isInfixOf mes x = stringParaDouble price : extractPrices rest mes
                                   | otherwise = extractPrices rest mes

maiorElemento :: [Double] -> Double
maiorElemento lista = maximum lista

menorElemento :: [Double] -> Double
menorElemento lista = minimum lista

stringParaDouble :: String -> Double
stringParaDouble str = read str


main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result