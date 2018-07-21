module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = parseMessage' (words line)
  where parseMessage' ("I":timestamp:rest) = LogMessage Info (readInt timestamp) (unwords rest)
        parseMessage' ("E":code:timestamp:rest) = LogMessage (Error (readInt code)) (readInt timestamp) (unwords rest)
        parseMessage' rest = Unknown (unwords rest)
        readInt a = read a :: Int

-- #1b
parse :: String -> [LogMessage]
parse multilines = map parseMessage (lines multilines)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m t = insertBy f m t
  where f (LogMessage _ ts _) = ts

insertBy :: (Ord a) => (LogMessage -> a) -> LogMessage -> MessageTree -> MessageTree
insertBy _ (Unknown _) t = t
insertBy _ m Leaf = Node Leaf m Leaf
insertBy f m (Node left m2 right)
  | f m == f m2 = Node left m2 right
  | f m < f m2 = Node (insertBy f m left) m2 right
  | f m > f m2 = Node left m2 (insertBy f m right)

-- #3
build :: [LogMessage] -> MessageTree
build messages = buildBy f messages
  where f (LogMessage _ ts _) = ts

buildBy :: (Ord a) => (LogMessage -> a) -> [LogMessage] -> MessageTree
buildBy f messages = buildBy' f messages Leaf
  where buildBy' f (x:xs) tree = buildBy' f xs (insertBy f x tree)
        buildBy' _ [] tree = tree
-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

-- #5
-- will return the messages from LogMessages with Errors whose severity 
-- is 50+ - sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map sf $ sortByTimestamp $ over50 $ errorMessages
  where errorMessages = getErrorMessages messages
        over50 errors = filter gte50 errors
        gte50 (LogMessage (Error code) _ _) = code >= 50
        gte50 (LogMessage _ _ _) = False
        sortByTimestamp = inOrder . buildBy (tsf)
        tsf (LogMessage _ ts _) = ts
        sf (LogMessage _ _ s) = s

getErrorMessages :: [LogMessage] -> [LogMessage]
getErrorMessages messages = filter (\x -> case x of LogMessage (Error _) _ _ -> True
                                                    LogMessage _ _ _ -> False) messages