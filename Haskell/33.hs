import Data.List 

type Frac = (Int,Int)

simple:: Frac -> Frac
simple (n,d)
  | d == 0 = (n,d)
  | factor == 1 = (n,d)
  | otherwise = simple (div n factor, div d factor)
    where factor = gcd n d

digits:: Int -> [Int]
digits x
  | x < 10       = [x]
  | otherwise = (mod x 10) : digits (div x 10)

digitsToInt:: [Int] -> Int
digitsToInt =sum . zipWith (*) (map (10 ^) [0..])

candidates:: [Frac]
candidates = [ (a,b) | a <- doubleDigits, b <- doubleDigits, a < b]
  where doubleDigits = map digitsToInt [[a,b] | a <- [1..9], b <- [1..9]]

fakeSimple:: Frac -> Frac
fakeSimple = unDigitize . simp . digitize
  where digitize (n,d) = (digits n, digits d)
        simp (n,d) = (n \\ d, d \\ n)
        unDigitize (n,d) = (digitsToInt n, digitsToInt d)

test:: Frac -> Bool
test x = simple x /= x && fakeSimple x /= x && (simple . fakeSimple) x == simple x

fractions:: [Frac]
fractions = filter test candidates

solve:: Int
solve =snd . simple $ foldl (\(n,d) (a,b) -> (n*a , d*b)) (1,1) fractions

-- *Main> solve
-- 100
-- (0.02 secs, 16306248 bytes)