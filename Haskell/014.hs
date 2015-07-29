collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
    | even n    = n : collatz (div n 2)
    | otherwise = n : collatz (3*n + 1)

solve :: Int
solve = snd . maximum $ zip (map (length . collatz) [500001,500003..999999]) [500001,500003..]

main = print solve

-- 837799
-- ./14  1.60s user 0.00s system 99% cpu 1.601 total
