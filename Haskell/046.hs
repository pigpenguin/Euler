import Control.Monad (liftM2)

primes = 2 : filter isPrime [3,5..]

divides x = (== 0) . mod x

isPrime n =  not . any (divides n) $  takeWhile (<= (floor . sqrt . fromIntegral $ n)) primes

composite = not . isPrime

both :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
both = liftM2 (&&)

toTest :: [Integer]
toTest = filter (both composite odd) [1..] 

squares :: [Integer]
squares = fmap ((*2) . (^2)) [1..]

works :: Integer -> Bool
works x = any isPrime $ takeWhile (> 0) $ map (x -) squares

solve = head . filter (not . works) $ toTest

main = print solve

-- time ./046 
-- 5777
-- ./046  0.00s user 0.00s system 72% cpu 0.005 total

