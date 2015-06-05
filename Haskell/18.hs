expand:: [Int] -> [Int]
expand [] = []
expand (a:[]) = [a]
expand (a:b:xs) = (max a b) : (expand(b:xs))

fix:: [Int] -> [Int]
fix [] = []
fix x = head x : expand x

solve:: [[Int]] -> Int
solve = maximum . foldl (\x acc -> zipWith (+) (fix x) (acc)) [0,0