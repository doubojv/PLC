type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa operation = calculadora operation 0
                    where 
                    calculadora [] result = result
                    calculadora ((c,v): t) result | c == "Multiplica" = calculadora t (result * v)
                                                  | c == "Soma" = calculadora t (result + v)
                                                  | c == "Subtrai" = calculadora t (result - v)
                                                  | (c == "Divide" && v /= 0)= calculadora t (div result  v)
                                                  | (c == "Divide" && v == 0) = -666

main = do
    a <- getLine
    let result = executa (read a)
    print result