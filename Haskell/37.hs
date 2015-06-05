import Data.List
import Data.Numbers.Primes

trunc:: Integer -> Bool
trunc x = all isPrime $ map read $ left ++ right
  where left = tail . inits $ show x
        right = tail . init . tails $ show x


possible x = filter trunc $ if isPrime x then x:xs else []
  where xs = concatMap (possible . ((10*x) +)) [1,3,7,9]
  
answer = sum . filter(>=23) $ concatMap possible [2,3,5,7]

main = do print $ answer

-- ghci> :main
-- 748332
-- (0.05 secs, 81880120 bytes)