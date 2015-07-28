squares = sum . map (^ 2) $ [1..100]

nums = sum [1..100]

solve = nums^2 - squares

-- *Main> solve
-- 25164150
-- (0.00 secs, 0 bytes)