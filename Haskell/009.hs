tris = [ [a,b,c] | m <- [2..lim], n <- [1..(m-1)], let a = m^2 - n^2, let b = 2*m*n, let c = m^2 + n^2, a+b+c == 1000]
  where lim = floor . sqrt . fromIntegral $ 1000

solve = product . head $ tris

-- *Main> solve
-- 31875000
-- (0.00 secs, 1830440 bytes)