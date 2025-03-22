{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module JoinList where

import Sized

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
-- mempty = Sum 0
-- mappend = (+)

-- newtype Product a = Product a
--     deriving (Eq, Ord, Num, Show)

-- getProduct :: Product a -> a
-- getProduct (Product a) = a

-- instance Num a => Monoid (Product a) where
--     mempty = Product 1
--     mappend = (*)


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
x +++ y = Append (tag x `mappend` tag y) x y

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

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ (Single a b) = Single a b
takeJ qty tree@(Append q left right)    | qty >= getSize (size q) = tree
                                        | qty <= getSize (size (tag left)) = Append q (takeJ qty left) Empty
                                        | otherwise = 
                                            let 
                                                quantity = (qty - getSize (size (tag left)))
                                            in
                                            Append q left (takeJ quantity right)

