import Data.List

prod:: [Int] -> [Int]
prod [] = []
prod x = (product (take 4 x) ): (prod $ tail x)

diagonal:: [Int] -> [Int]
diagonal [] = []
diagonal xs = head xs : diagonal (drop 21 xs)

diagonals:: [Int] -> [[Int]]
diagonals xs 
  | length xs > 20 = diagonal xs : diagonals (tail (take ((length xs) -20) xs))
  | otherwise = []


solve:: [[Int]] -> Int
solve = maximum . map (maximum . prod) 

