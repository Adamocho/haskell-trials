{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- Ex 1
fib :: Integer -> Integer
fib n | n <= 0 = 0
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [ fib x | x <- [0..] ]

-- Ex 2
fibs2 :: [Integer]
-- fibs2 = foldl fib2 [] [0..]
-- fibs2 = foldl' fib3 [] $ repeat 7
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

fibs3 :: [Integer]
fibs3 = 0 : 1 : further fibs3
      where
            further (x : rest@(y:_)) = (x + y) : further rest

-- this code here doesn't work
-- fib3 :: [Integer] -> Integer -> [Integer]
-- fib3 x y = y : x

-- fib2 :: [Integer] -> Integer -> [Integer]
-- fib2 [] _               = [0]
-- fib2 list@[_] _         = list ++ [1]
-- fib2 list _ = case unsnoc list of
--       Nothing -> [0]
--       (Just ([], _)) -> [1]
--       (Just (xs, first)) -> case unsnoc xs of
--             Nothing -> [1]
--             (Just (_, second)) -> list ++ [first + second] 


-- Ex 3
data Stream a = a :> Stream a

streamToList :: Stream a -> [a]
streamToList (a :> restofA) = a : streamToList restofA

instance Show a => Show (Stream a) where
      show (a :> rest) = show $ take 15 $ a : streamToList rest

-- Ex 4
streamRepeat :: a -> Stream a
streamRepeat x = x :> streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :> xs) = f x :> streamMap f xs

streamSeed :: (a -> a) -> a -> Stream a
streamSeed f x = x :> streamSeed f (f x)

-- Ex 5
nats :: Stream Integer
nats = streamSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :> xs) second = x :> interleaveStreams second xs

ruler :: Stream Integer
ruler = ruling 0
      where
            ruling n = interleaveStreams (streamRepeat n) (ruling (n + 1))


-- Ex 6
x :: Stream Integer
x = 0 :> streamSeed (*0) 1


instance Num (Stream Integer) where
      fromInteger n = n :> streamRepeat 0
      negate (x :> xs) = (-x) :> negate xs
      (+) (x :> xs) (y :> ys) = (x + y) :> (+) xs ys
      (*) (x :> xs) right@(y :> ys) = (x * y) :> (+) (streamMap (x*) ys) ((*) xs right)

instance Fractional (Stream Integer) where
      (/) a@(x :> xs) b@(y :> ys) = (x `div` y) :> streamMap ( `div` y) (xs - (*) ((/) a b) ys)

fibstream :: Stream Integer
fibstream = x / (1 - x - x ^ 2)

