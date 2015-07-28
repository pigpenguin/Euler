solve = last . factorise $ 600851475143 

divideOut num fact
  | num `mod` fact == 0 = divideOut (num `div` fact) fact
  | otherwise = num

factorise:: Int -> [Int]
factorise num = 1 : (fhelp num 2)

fhelp num last
    | num == 1 = []
    | num `mod` next == 0 = next : fhelp (divideOut num next) next
    | otherwise = fhelp num next
      where next = last + 1

-- *Main> solve
-- 6857
-- (0.00 secs, 0 bytes)