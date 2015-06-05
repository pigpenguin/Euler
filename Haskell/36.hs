import Data.Char (intToDigit)
import Numeric (showIntAtBase)

palindromic::(Eq a)=>[a] -> Bool
palindromic x = reverse x == x

binary:: Int -> String
binary x = showIntAtBase 2 intToDigit x ""

main = print $ sum $ filter (\x -> (palindromic $ show x) && (palindromic $ binary x)) [1,3..999999]