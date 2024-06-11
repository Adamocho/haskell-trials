-- Homework 1

-- Bank Card Number Validator
-- Exercise 1
toDigitsRev    :: Integer -> [Integer]
toDigitsRev -x = []
toDigitsRev 0 = []
toDigitsRev x = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits    :: Integer -> [Integer]
toDigits xs = reverse (toDigitsRev xs)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:[]) = [x, y * 2]
doubleEveryOtherRev (x:y:xs) = x : (y * 2) : (doubleEveryOtherRev xs)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool 
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- Hanoi Tower Move Calculator
-- Exercise 5
type Peg = String
type Move = (Peg, Peg) 
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, c)]
hanoi 2 a b c = [(a, b), (a, c), (b, c)]
hanoi x a b c = hanoi (subtract 1 x) a c b ++ hanoi 1 a b c ++ hanoi (subtract 1 x) b a c

-- Exercise 6??? 
introduce hanoi with 4 pegs
