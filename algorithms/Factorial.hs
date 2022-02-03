module Factorial where

main :: IO ()
main = do
    print $ factorial 9

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1

proto :: Integer -> Integer
proto 0 = 20
proto 1 
    | 7 > 5 = 40
    | otherwise = 60
proto n 
    | n < 0 = 0
    | n `mod` 2 == 0 = 80
    | otherwise = 100

sumArr :: [Integer] -> [Integer]
sumArr [] = []
sumArr (x:[]) = [x]    
sumArr (x:y:zs) = (x + y) : sumArr zs