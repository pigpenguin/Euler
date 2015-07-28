fibs = 0 : scanl (+) 1 fibs

solve = sum . filter (\x -> x `mod` 2 == 0) $ takeWhile (< 4000000) fibs

-- *Main> solve
-- 4613732
-- (0.00 secs, 0 bytes)