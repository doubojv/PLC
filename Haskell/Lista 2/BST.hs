data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Eq, Ord, Read)


preOrder :: Tree t -> [t]
preOrder Nilt = []
preOrder (Node valor rightTree leftTree) = [valor] ++ preOrder rightTree ++ preOrder leftTree              
              

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node op Nilt Nilt) = True
isBST (Node op rightTree Nilt) | (foldr (max) op(preOrder rightTree) == op)  && isBST rightTree = True
                               | otherwise = False

isBST (Node op Nilt leftTree) | (foldr (min) op(preOrder leftTree) == op) && isBST leftTree = True
                              | otherwise = False

isBST (Node op rightTree leftTree) | (foldr (max) op(preOrder rightTree) == op) &&  (foldr (min) op(preOrder leftTree) == op) && isBST rightTree && isBST leftTree = True
                                   | otherwise = False

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result

