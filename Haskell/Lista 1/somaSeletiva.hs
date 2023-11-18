sumNumbers :: [Char] -> Int
sumNumbers word = function word 0 0
    where 
        function [] total current = total + current
        function (x:xs) total current | x >= '0' && x <= '9' = 
            let realcurrent = fromEnum x - fromEnum '0'
            in if current /= 0
               then function xs (total + current) realcurrent
               else 
                function xs total realcurrent
                                      | current /= 0 = function xs (total + current) 0 
                                      | otherwise = function xs total 0

main = do
  a <- getLine
  let result = sumNumbers a
  print result