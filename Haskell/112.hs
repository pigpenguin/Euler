digits:: Integer -> [Int]
digits = map (read . return) . show

increasing:: [Int] -> Bool
increasing (a:b:[]) = a <= b
increasing (a:b:cs) = a <= b && increasing (b:cs)
increasing x = True

decreasing:: [Int] ->Bool
decreasing (a:b:[]) = a >= b
decreasing (a:b:cs) = a >= b && decreasing (b:cs)
decreasing x= True

bouncy:: Integer -> Bool
bouncy x = not (increasing y || decreasing y)
  where y = digits x

meh :: [Integer]
meh = foldr (\x acc -> (if x then last acc else (+ 1) $ last acc):acc) [0] (map bouncy [1..])
