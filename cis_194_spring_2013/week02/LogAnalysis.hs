{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage x = case words x of
    "I":time:text -> LogMessage Info (read time :: Int) (unwords text)
    "W":time:text -> LogMessage Warning (read time :: Int) (unwords text)
    "E":sev:time:text -> LogMessage (Error (read sev :: Int)) (read time :: Int) (unwords text)
    _ -> Unknown x

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ time _) (Node leftTree middleLog rightTree)
    | time >= (getTime middleLog) = Node leftTree middleLog (insert message rightTree)
    | otherwise = Node (insert message leftTree) middleLog rightTree

getTime :: LogMessage -> Int
getTime (Unknown _) = 0
getTime (LogMessage _ time _) = time

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf node Leaf) = [node]
inOrder (Node left node right) = (inOrder left) ++ [node] ++ (inOrder right)
