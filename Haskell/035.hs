import Data.List (nub,sort)

primes = 2 : [x | x <- [3..], isprime x]

isprime x = all (\p  -> x `mod` p > 0) (factorsToTry x)
  where
    factorsToTry x = takeWhile (\p -> p*p <= x) primes

-- primes::[IO Integer]
-- primes = readFile "primes.txt" >>= return . words >>= return . map read

candidates:: [[Int]]
candidates =  [2] : [5] : (filter (all (`notElem` badDigits)) $ map digits $ takeWhile (< 1000000) primes)
  where badDigits = [0,2,4,5,6,8]
        digits = map (read . return) . show

circle:: (Ord a) => [a] -> [[a]]
circle x = nub $ sort [(iterate chain x) !! n | n <- [1..length x]]
  where chain n = tail $ take ((+1) (length n)) $ cycle n