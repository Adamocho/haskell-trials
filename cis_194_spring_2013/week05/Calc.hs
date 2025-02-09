{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT as E
import Parser
import StackVM as S

-- Ex 1
eval :: ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add one two) = eval one + eval two
eval (E.Mul one two) = eval one * eval two


-- Ex 2
evalStr :: String -> Maybe Integer
evalStr x = case parseExp E.Lit E.Add E.Mul x of
    Nothing -> Nothing
    Just z -> Just (eval z)


-- Ex 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: ExprT -> ExprT
reify = id

-- Ex 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit a
        | a <= 0 = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
-- testInteger = testExp :: Maybe Integer
-- testBool = testExp :: Maybe Bool
-- testMM = testExp :: Maybe MinMax
-- testSat = testExp :: Maybe Mod7
testCPU = testExp :: Maybe Program


-- Ex 5
instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
