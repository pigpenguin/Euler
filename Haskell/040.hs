import Data.Char (digitToInt)

champ:: String
champ = concatMap show [1..]

index::[Int]
index = map (\x -> (10^x)-1) [0..6]

solution::Int
solution = product $ map digitToInt $ map (champ !! ) index

-- ghci> solution
-- 210
-- (0.08 secs, 113845232 bytes)