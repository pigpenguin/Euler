solve = sum . filter (multiple) $ [1..999]
  where multiple x = (x `mod` 3 == 0) || (x `mod` 5 == 0)
  
-- *Main> solve
-- 233168
-- (0.00 secs, 0 bytes)