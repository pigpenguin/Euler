coprime:: Int -> Int -> Bool
coprime x y = gcd x y == 1

prims = [ [a,b,c] | m <- [2..22], n <- [1..(m-1)], coprime m n , odd (m-n), let a = m^2 - n^2, let b = 2*m*n, let c = m^2 + n^2, a+b+c <= 1000]

sols x = filter ((== 0) . (x `mod`) . sum ) prims

solve = snd . maximum $ zip s [1..]
  where s = map (length . sols) [1..1000]

-- *Main> solve
-- 840
-- (0.02 secs, 13520416 bytes)