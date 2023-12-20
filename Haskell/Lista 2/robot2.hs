data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces direction [] = direction
faces direction (Forward z:xs) = faces direction xs
faces direction (Backward z:xs) = faces direction xs

faces North (x:xs) | x == TurnLeft = faces West xs
                   | x == TurnRight = faces East xs

faces East (x:xs)  | x == TurnLeft = faces North xs
                   | x == TurnRight = faces South xs

faces South (x:xs) | x == TurnLeft = faces East xs
                   | x == TurnRight = faces West xs         
                   
faces West (x:xs)  | x == TurnLeft = faces South xs
                   | x == TurnRight = faces North xs


main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result