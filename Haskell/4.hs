products = [x*y | x <- [100..999] , y <- [100..999]]

palindrome x = (show x) == (reverse . show $ x)

solve = maximum . filter palindrome $ products

-- *Main> solve
-- 906609
-- (1.23 secs, 914429024 bytes)