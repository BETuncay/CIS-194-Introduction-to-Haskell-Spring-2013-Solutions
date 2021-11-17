{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


-- Exercicse 1
parseMessage :: String -> LogMessage
parseMessage msg 
   | take 1 (words msg) == ["I"]  = 
     LogMessage Info
                (read (unwords (drop 1 (take 2 (words msg)))) :: Int)
                (unwords (drop 2 (words msg)))
   | take 1 (words msg) == ["W"]  = 
     LogMessage Warning
                (read (unwords (drop 1 (take 2 (words msg)))) :: Int)
                (unwords (drop 2 (words msg)))
   | take 1 (words msg) == ["E"]  = 
     LogMessage (Error (read (unwords (drop 1 (take 2 (words msg)))) :: Int))
                (read (unwords (drop 2 (take 3 (words msg)))) :: Int)
                (unwords (drop 3 (words msg)))
   | otherwise = Unknown msg

parse :: String -> [LogMessage]
parse msg = map parseMessage (lines msg)


-- Exercicse 2
insert :: LogMessage -> MessageTree -> MessageTree 
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert _ (Node _ (Unknown _) _) = Leaf -- error
insert (LogMessage newX newValue newY) (Node ltree (LogMessage x value y) rtree) = 
  if newValue < value
  then Node (insert (LogMessage newX newValue newY) ltree) (LogMessage x value y) rtree
  else Node ltree (LogMessage x value y) (insert (LogMessage newX newValue newY) rtree)


-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build x = insert (last x) (build (init x))


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree value rtree) = inOrder ltree ++ [value] ++ inOrder rtree


-- Exercise 5
getErrors :: LogMessage -> Bool
getErrors (LogMessage (Error val) _ _) = val >= 50
getErrors _ = False

getLogString :: LogMessage -> String
getLogString (LogMessage _ _ str) = str
getLogString (Unknown str) = str

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = map getLogString (filter getErrors (inOrder (build logList)))
