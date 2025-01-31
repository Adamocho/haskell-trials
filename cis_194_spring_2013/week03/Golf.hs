{-# OPTIONS_GHC -Wall #-}

module Golf where
import Data.List

-- Task 1 --
{-
skips takes a list and transofrms it into list of lists, where each of
the lists represents every n-th letter
-}
skips :: [a] -> [[a]]
skips xs = [ skip n xs | n <- [1..(length xs)]]

skip :: Int -> [a] -> [a]
-- skip n xs = case splitAt (n - 1) xs of (_,[]) -> []; (_,x:_) -> x : skip n (drop n xs)
skip n xs = case splitAt (n - 1) xs of 
    (_,[]) -> []
    (_,x:_) -> x : skip n (drop n xs)

-- Task 2 --
{-
It looks for a triplet of values and then checks the condition, after that it
makes a recursive call with a smaller list
-}
localMaxima :: [Integer] -> [Integer]
-- localMaxima (x:y:z:xs) = if (y > x) && (y > z) then y : localMaxima (y:z:xs) else localMaxima (y:z:xs)
localMaxima (x:y:z:xs) 
    | (y > x) && (y > z) = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

-- Task 3 --
{-
How it works:
Splits list into [0..9] x (1 digit for each) sections and then
checks if x in [0..9] is in this sublist, rendering a '*' or ' '
and then append the rest of the string.
Run with: putStr (histogram [7,4])
-}
histogram :: [Integer] -> String
-- histogram numbers = concat [ '\n' : [ if x `elem` xs then '*' else ' ' | x <- [0..9] ] | xs <- reverse (createHistogram numbers) ] ++ "\n==========\n0123456789\n"
histogram numbers = concat 
    [ '\n' : 
        [ if x `elem` xs then '*' else ' ' | x <- [0..9] ] 
    | xs <- reverse (createHistogram numbers) ]
    ++ "\n==========\n0123456789\n"

createHistogram :: [Integer] -> [[Integer]]
createHistogram [] = []
createHistogram xs = [x | x <- [0..9], x `elem` xs] : createHistogram (xs \\ [ x | x <- [0..9], x `elem` xs])
