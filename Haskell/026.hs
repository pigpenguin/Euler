ord x
    | divs x == True = 0
    | otherwise = (+1) $ length $ takeWhile (/=1) $ map ((`mod` x) . (10 ^))  [1..]
    where divs x = (x `mod` 2 == 0 ) || (x `mod` 5 == 0)

-- *Main> snd $ maximum (zip (map ord [2..999]) [2..])
-- 983
-- (0.11 secs, 209001504 bytes)