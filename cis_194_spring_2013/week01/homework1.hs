{-# OPTIONS_GHC -Wall #-}

-- this checks if the supplied bank account number is correct or not --
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = mod x 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs

-- sum EACH digit of the number present in the list --
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
    | x > 9 = x `mod` 10 + x `div` 10
    | otherwise = x
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

-- hanoi tower --
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start _ end = [(start, end)]
hanoi n start middle end = hanoi (n - 1) start end middle ++ [(start, end)] ++ hanoi (n - 1) middle start end
