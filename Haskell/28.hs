diagonals = scanl (+) 1 (concatMap (replicate 4) [2,4..1000])
solve = sum diagonals

-- *Main> solve
-- 669171001
--(0.00 secs, 1130400 bytes)