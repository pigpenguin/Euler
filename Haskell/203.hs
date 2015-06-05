import Data.Numbers.Primes (primes)
import Data.List (nub)

expand:: [Int] -> [Int]
expand [] = []
expand (a:[]) = [a]
expand (a:b:xs) = (a+b) : (expand(b:xs))

fix:: [Int] -> [Int]
fix [] = []
fix x = head x : expand x

pascals = iterate fix [1]

squareFree:: (Integral int)=> int -> Bool
squareFree x = all (\p -> x `mod` (p*p) /= 0) (takeWhile (<= 51) primes)

main = do print $ sum $ filter squareFree $ nub $ concat $ take 51 pascals

-- *Main Data.List> :main
-- 34029210557338
-- (0.05 secs, 14535880 bytes)