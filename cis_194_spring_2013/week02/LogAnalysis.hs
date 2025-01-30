{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage x = case words x of
    "I":time:text -> LogMessage Info (read time :: Int) (unwords text)
    "W":time:text -> LogMessage Warning (read time :: Int) (unwords text)
    "E":sev:time:text -> LogMessage (Error (read sev :: Int)) (read time :: Int) (unwords text)
    _ -> Unknown x

parse :: String -> [LogMessage]
-- parse xs = [ parseMessage x | x <- lines xs ]
parse x = map parseMessage (lines x)

insert :: LogMessage -> MessageTree -> MessageTree
insert logm Leaf = Node Leaf logm Leaf
insert toAdd@(LogMessage _ timestamp _) (Node left middle@(LogMessage _ ts _) right)
    | timestamp < ts = Node (insert toAdd left) middle right
    | otherwise = Node left middle (insert toAdd right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node leftTree middle rightTree) = inOrder leftTree ++ [middle] ++ inOrder rightTree
inOrder Leaf = []

whatType :: LogMessage -> String
whatType (LogMessage (Error _) _  _) = "Error"
whatType (LogMessage Info _  _) = "Info"
whatType (LogMessage Warning _  _) = "Warning"
whatType (Unknown _) = "Unkonwn"

severity :: LogMessage -> Int
severity (LogMessage (Error sev) _  _) = sev
severity _ = 0

extractMessages :: [LogMessage] -> [String]
extractMessages [] = []
extractMessages ((LogMessage _ _ message):xs) = message : extractMessages xs
extractMessages ((Unknown _):xs) = extractMessages xs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = extractMessages (inOrder (build [ x | x <- xs, whatType x == "Error", severity x >= 50]))
-- whatWentWrong xs = extractMessages (inOrder (build [ x | x <- xs, whatType x == "Error" ]))
