helper:: Int -> Int -> Bool
helper tested factor
  | factor > (floor . sqrt $ fromIntegral tested) = True
  | tested `mod` factor == 0 = False
  | otherwise = helper tested (factor + 2)
  
isPrime x 
  | x == 2 = True
  | x `mod` 2 == 0 = False
  | otherwise = helper x 3
  
primes = filter isPrime [2..]

solve = primes !! 10001

-- *Main> solve
-- 104743
-- (2.53 secs, 1430117456 bytes)