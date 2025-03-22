{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}


module JoinList where

import Sized
import Scrabble
import Buffer

-- data JoinListBasic a = Empty
--                       | Single a
--                       | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- newtype Sum a = Sum a
--     deriving (Eq, Ord, Num, Show)

-- getSum :: Sum a -> a
-- getSum (Sum a) = a

-- instance Num a => Monoid (Sum a) where
--     mempty = Sum 0

-- instance Num a => Semigroup (Sum a) where
--     Sum x <> Sum y = Sum (x + y)

-- newtype Product a = Product a
--     deriving (Eq, Ord, Num, Show)

-- getProduct :: Product a -> a
-- getProduct (Product a) = a

-- instance Num a => Monoid (Product a) where
--     mempty = Product 1

-- instance Num a => Semigroup (Product a) where
--     Product x <> Product y = Product (x * y)


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x = x
x +++ Empty = x
-- (Single m1 a1) +++ (Single m2 a2) = Append (m1 `mappend` m2) (Single m1 a1) (Single m2 a2)
-- append@(Append m1 _ _) +++ single@(Single m2 _) = Append (m1 `mappend` m2) append single
-- single@(Single m1 _) +++ append@(Append m2 _ _) = Append (m1 `mappend` m2) single append
-- append1@(Append m1 _ _) +++ append2@(Append m2 _ _) = Append (m1 `mappend` m2) append1 append2
x +++ y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                                                      = Nothing
indexJ index (Single _ a)                           | index > 0     = Nothing
                                                    | otherwise     = Just a
indexJ index (Append m left@(Append m1 _ _) right)  | index >= getSize (size m)    = Nothing
                                                    | index < getSize (size m1)    = indexJ index left
                                                    | otherwise     = indexJ (getSize (size m) - getSize (size m1)) right
indexJ index (Append _ left@(Single _ _) right)     | index > 0     = indexJ (index - 1) right
                                                    | otherwise     = indexJ (index - 1) left
indexJ index (Append _ Empty right)                 | index >= getSize (size (tag right)) = Nothing
                                                    | otherwise     = indexJ index right

-- dropJ :: (Sized b, Monoid b, Num b) => Int -> JoinList b a -> JoinList b a
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 x = x
dropJ qty (Single a b)          | qty == 0       = Single a b
                                | otherwise    = Empty
dropJ qty (Append q (Single _ _) right)  | qty > 0 = Append q Empty (dropJ (qty - 1) right)
-- dropJ qty (Append q (Single a _) right)  | qty > 0 = Append (q - a) Empty (dropJ (qty - 1) right)
dropJ qty (Append q Empty right)           = Append q Empty (dropJ qty right)
dropJ qty (Append q left right)         | qty >= getSize (size q) = Empty
                                        | qty <= getSize (size (tag left)) = Append q (dropJ qty left) right
                                        | otherwise =
                                            let
                                                quantity = (qty - getSize (size (tag left)))
                                            in
                                            Append q Empty (dropJ quantity right)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ qty (Single a b)                  | qty == 0  = Empty
                                        | otherwise = Single a b
takeJ qty tree@(Append q left right)    | qty >= getSize (size q) = tree
                                        | qty <= getSize (size (tag left)) = Append q (takeJ qty left) Empty
                                        | otherwise =
                                            let
                                                quantity = (qty - getSize (size (tag left)))
                                            in
                                            Append q left (takeJ quantity right)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
-- mempty = (mempty, mempty)

-- instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
-- mappend (a1,b1) (a2,b2) = (a1 <> a2, b1 <> b2)

instance Buffer (JoinList (Score, Size) String) where
    toString :: JoinList (Score, Size) String -> String
    toString Empty = ""
    toString (Single _ str) = str
    toString (Append _ l r) = toString l ++ toString r

    fromString :: String -> JoinList (Score, Size) String
    fromString str = Single (scoreString str, mempty) str

    line :: Int -> JoinList (Score, Size) String -> Maybe String
    line = indexJ

    replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
    replaceLine _ _ Empty = Empty
    replaceLine index str single@(Single _ _)   | index == 0 = fromString str
                                                | otherwise = single
    replaceLine index str tree@(Append (sc, si) left right) 
            | index >= getSize si = tree
            | index >= getSize (size (tag left)) = 
                Append (sc, si) left (replaceLine (index - getSize (size (tag left))) str right)
            | otherwise = 
                Append (sc, si) (replaceLine index str left) right

    numLines :: JoinList (Score, Size) String -> Int
    numLines Empty = 0
    numLines (Single (_, sz) _) = getSize sz
    numLines (Append (_, sz) _ _) = getSize sz

    value :: JoinList (Score, Size) String -> Int
    value Empty = 0
    value (Single (sc, _) _) = getScore sc
    value (Append (sc, _) _ _) = getScore sc

myString :: String
myString = "This buffer is for notes you don't want to save, and for"

start :: JoinList (Score, Size) String
start = Single (scoreString myString, Size 0) myString
