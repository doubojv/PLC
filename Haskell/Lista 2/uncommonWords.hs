import Data.Char (toLower)
import Data.List (sort, nub)

uncommonWords :: String -> String -> [String]
uncommonWords "" "" = []
uncommonWords (x:xs) "" = [xs]
uncommonWords "" (y:ys) = [ys]
uncommonWords (x:xs) (y:ys) | toLower x == toLower y = uncommonWords xs ys
                            | otherwise = nub (sort ([map toLower $ takeWhile (/= ' ') (x:xs)] ++ [map toLower $ takeWhile (/= ' ') (y:ys)] ++ uncommonWords (dropWhile (/= ' ') xs) (dropWhile (/= ' ') ys)))

main :: IO ()
main = do
  sentence1 <- getLine
  sentence2 <- getLine
  let result = uncommonWords sentence1 sentence2
  print result
