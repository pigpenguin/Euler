primes = 2 : filter isPrime [3..]

divides x = (== 0) . mod x

isPrime n =  not . any (divides n) $  takeWhile (<= (floor . sqrt . fromIntegral $ n)) primes

solve = sum . takeWhile (<2000000) $ primes

main = print solve

-- 142913828922
-- ./10  1.20s user 0.00s system 99% cpu 1.203 total

