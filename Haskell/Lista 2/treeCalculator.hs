data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node op firstOp secOp) = operation op firstOp secOp

operation :: Ops -> IntTree -> IntTree -> Int
operation SUM first sec = evalTree first + evalTree sec
operation SUB first sec = evalTree first - evalTree sec
operation MUL first sec = evalTree first * evalTree sec


main = do
    s <- getLine
    let result = evalTree (read s)
    print result