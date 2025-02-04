{-# OPTIONS_GHC -Wall #-}

-- Ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
-- wholemeal version of fun1
fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . subtract 2) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
-- wholemeal version of fun2
fun2' :: Integer -> Integer
fun2' = sum . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- Ex 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode a Leaf = Node 0 Leaf a Leaf
addNode a (Node depth Leaf value right) = Node (depth + 1) (addNode a Leaf) value right
addNode a (Node depth left value Leaf) = Node depth left value (addNode a Leaf)
addNode a (Node _ left value right) =
    if minDepthOf left <= minDepthOf right
    then
        let maxDepth = depthOf (Node maxDepth (addNode a left) value right)
        in
        Node maxDepth (addNode a left) value right
    else
        let maxDepth = depthOf (Node maxDepth left value (addNode a right))
        in
        Node maxDepth left value (addNode a right)

depthOf :: Tree a -> Integer
depthOf (Node _ Leaf _ _) = 0
depthOf (Node _ left@Node{} _ _) = 1 + depthOf left
depthOf _ = 0

minDepthOf :: Tree a -> Integer
minDepthOf Leaf = 0
minDepthOf (Node _ Leaf _ Leaf) = 0
minDepthOf (Node _ Node{} _ Leaf) = 0
minDepthOf (Node _ Leaf _ Node{}) = 0
minDepthOf (Node _ left _ right) = 1 + min (minDepthOf left) (minDepthOf right)


-- Ex 3
xor :: [Bool] -> Bool
xor = odd . foldr (\x -> if x then ((1 :: Integer)+) else (0+)) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> (f x :)) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x -> (`f` x)) base (reverse xs)


-- Ex 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 1 + 2 * x) . filterOut . cartesianProduct

-- cartesianProduct :: (Ord a, Num a, Enum a) => a -> [(a, a)]
cartesianProduct :: Integer -> (Integer, [Integer])
cartesianProduct limit = (limit, [ x + y + 2 * x * y | x <- [1 .. limit], 1 <= x, y <- [1 .. limit], x <= y, x + y + 2 * x * y <= limit ])

filterOut :: (Integer, [Integer]) -> [Integer]
filterOut (limit, xs) = [ x | x <- [1..limit], x `notElem` xs ]
