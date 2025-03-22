{-# OPTIONS_GHC -Wall #-}

module Scrabble where

newtype Score = Score Int
    deriving (Show, Eq, Ord, Num)

instance Semigroup Score where
    Score x <> Score y = Score (x + y)

instance Monoid Score where
    mempty = Score 0

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score ch    | ch == 'a' = Score 1
            | ch == 'b' = Score 3
            | ch == 'c' = Score 3
            | ch == 'd' = Score 2
            | ch == 'e' = Score 1
            | ch == 'f' = Score 4
            | ch == 'g' = Score 2
            | ch == 'h' = Score 4
            | ch == 'i' = Score 1
            | ch == 'j' = Score 8
            | ch == 'k' = Score 5
            | ch == 'm' = Score 1
            | ch == 'n' = Score 3
            | ch == 'l' = Score 1
            | ch == 'o' = Score 1
            | ch == 'p' = Score 3
            | ch == 'q' = Score 10
            | ch == 'r' = Score 1
            | ch == 's' = Score 1
            | ch == 't' = Score 1
            | ch == 'u' = Score 1
            | ch == 'v' = Score 4
            | ch == 'w' = Score 4
            | ch == 'x' = Score 8
            | ch == 'y' = Score 4
            | ch == 'z' = Score 10
            | otherwise = Score 0

scoreString :: String -> Score
scoreString str = sum [score x | x <- str]


