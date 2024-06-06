-- Homework 1


-- Exercise 1
toDigitsRev    :: Integer -> [Integer]
toDigits -x = []
toDigits 0 = []
toDigits x = (x `mod` 10) : toDigits (x `div` 10)

toDigits    :: Integer -> [Integer]
toDigitsRev xs = reverse (toDigits xs)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = 
doubleEveryOther []