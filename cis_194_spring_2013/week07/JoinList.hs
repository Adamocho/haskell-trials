{-# OPTIONS_GHC -Wall #-}

module JoinList where

-- data JoinListBasic a = Empty
--                       | Single a
--                       | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x = x
x +++ Empty = x
(Single m1 a1) +++ (Single m2 a2) = Append (m1 `mappend` m2) (Single m1 a1) (Single m2 a2)
append@(Append m1 _ _) +++ single@(Single m2 _) = Append (m1 `mappend` m2) append single
single@(Single m1 _) +++ append@(Append m2 _ _) = Append (m1 `mappend` m2) single append
append1@(Append m1 _ _) +++ append2@(Append m2 _ _) = Append (m1 `mappend` m2) append1 append2



