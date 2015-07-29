import qualified Data.IntSet as S
import Control.Monad (liftM2)

pairs :: [(Int,Int)]
pairs = liftM2 (,) [1..3000] [1..3000]

penta :: Integral n => n -> n
penta n = n * (3*n - 1) `div` 2

pentas :: Integral n => [n]
pentas = fmap penta [1..]

pentaSet :: Int -> S.IntSet
pentaSet = S.fromList . flip take pentas

isPenta :: Int -> Bool
isPenta = flip S.member $ pentaSet 3000

add :: (Int,Int) -> Int
add (a,b) = penta a + penta b

sub :: (Int,Int) -> Int
sub (a,b) = penta (max a b) - penta (min a b)

works :: (Int,Int) -> Bool
works p = (isPenta . add $ p) && (isPenta . sub $ p)

solve = sub . head . filter works $ pairs

main = print solve

-- time ./44
-- 5482660
-- ./44  0.22s user 0.00s system 99% cpu 0.224 total
