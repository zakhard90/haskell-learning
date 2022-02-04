module CreditCardCheckdigit where

{-
1. Given a number, drop the last digit (checkdigit)
2. Reverse the rest
3. Multiply the digits in odd positions (1,3,5 .. ) by 2
4. Subtract 9 to all results > 9
5. Sum all numbers
6. Append checkdigit
7. Check if the total can be divided by 10
 -}    

main :: IO ()
main = do 
    let x = [5,1,0,5,1,0,5,1,0,5,1,0,5,1,0,0]
    print $ validate x

validate :: [Int] -> Bool
validate xs = dTen . (+) (last xs) . (*) 10 . sum . sub9 . doubleOdd . reverse $ init xs 

-- Check if can be divided by 10
dTen :: Int -> Bool
dTen x = x `mod` 10 == 0

-- Multiply odd positions by 2
doubleOdd :: [Int] -> [Int]
doubleOdd [] = []
doubleOdd [x] = [x]
doubleOdd (x:y:xs) = x * 2 : y : doubleOdd xs

-- Sub 9 from > 9
sub9 :: [Int] -> [Int]
sub9 [] = []  
sub9 (x:xs) = if x > 9 then (x - 9) : sub9 xs else x : sub9 xs