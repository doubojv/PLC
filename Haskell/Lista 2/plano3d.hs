data Mov = Z Double | X Double | Y Double
                  deriving (Eq, Show, Read)

type Point3D = (Double, Double, Double)

type Object3D = [Point3D]

transladaObjeto :: Object3D -> [Mov] -> Object3D
transladaObjeto [] _ = []  
transladaObjeto obj [] = obj 
transladaObjeto ((a, b, c):xs) (X numx : Y numy : Z numz : rest) = (a + numx, b + numy, c + numz) : transladaObjeto xs (X numx : Y numy : Z numz : rest)
           

rotacionaX :: Double -> Object3D -> Object3D
rotacionaX angX [] = []
rotacionaX angX ((a,b,c):xs) = (a,b*(cos angX)-c*(sin angX),c*(cos angX)+b*(sin angX)) : rotacionaX angX xs


rotacionaY :: Double -> Object3D -> Object3D
rotacionaY angY [] = []
rotacionaY angY ((a,b,c):xs) = (a*(cos angY) + c*(sin angY),b,c*(cos angY) - a*(sin angY)) : rotacionaY angY xs

rotacionaZ :: Double -> Object3D -> Object3D
rotacionaZ angZ [] = []
rotacionaZ angZ ((a,b,c):xs) = (a*(cos angZ) - b*(sin angZ),a*(sin angZ)+b*(cos angZ),c) : rotacionaZ angZ xs


rotacionaObjeto :: Double -> Double -> Double -> Object3D -> Maybe Object3D
rotacionaObjeto angX angY angZ ((a,b,c):xs) | angX == -1 || angY == -1 || angZ == -1 = Nothing
                                            | otherwise = Just (rotacionaX (toRad angX) (rotacionaY (toRad angY) (rotacionaZ (toRad angZ) ((a,b,c):xs))))



toRad :: Double -> Double
toRad degrees = degrees * pi / 180


main = do
       coord <- getLine
       mov <- getLine
       angX <- getLine
       angY <- getLine
       angZ <- getLine
       let transladado = transladaObjeto (read coord :: Object3D) (read mov :: [Mov])
       let rotacionado = rotacionaObjeto (read angX) (read angY) (read angZ) transladado
       print rotacionado