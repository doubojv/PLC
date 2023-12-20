data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

        
maiorDiametro :: Ord t => Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node _ leftSubtree rightSubtree) =
      max (altura leftSubtree + altura rightSubtree + 1) (max (maiorDiametro leftSubtree) (maiorDiametro rightSubtree))

altura :: Tree t -> Int
altura Nilt = 0
altura (Node _ leftSubtree rightSubtree) = 1 + max (altura leftSubtree) (altura rightSubtree)

main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result