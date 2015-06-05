import Data.List

pete = [[a,b,c,d,e,f,g,h,i] | a <- [1..4] , b <- [1..4] , c <- [1..4] , d <- [1..4] , e <- [1..4] , f <- [1..4] , g <- [1..4] , h <- [1..4] , i <- [1..4] ]
colin = [[a,b,c,d,e,f] | a <- [1..6] , b <- [1..6] , c <- [1..6] , d <- [1..6] , e <- [1..6] , f <- [1..6] ]

scores = map (\x -> (head x, length x)) . group . sort . map sum 


outcomes::[(Bool,Int)]
outcomes = concatMap (\(s,a) -> map (\(p, n) -> (p > s, a*n)) $ scores pete) $ scores colin

wins = sum . map snd $ filter fst outcomes 
total = length pete * length colin

-- ghci> fromIntegral wins / fromIntegral total
-- 0.5731440767829801
-- (14.96 secs, 16033673216 bytes)