{-# OPTIONS_GHC -Wall -Werror #-}

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

import Data.List (foldl')
import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage line = case words line of
    "I":t:rest -> case reads t of
        [(t',"")] -> LogMessage Info t' (unwords rest)
        _ -> Unknown line
    "W":t:rest -> case reads t of
        [(t',"")] -> LogMessage Warning t' (unwords rest)
        _ -> Unknown line
    "E":n:t:rest -> case (reads n, reads t) of
        ([(n',"")],[(t',"")]) -> LogMessage (Error n') t' (unwords rest)
        _ -> Unknown line
    _ -> Unknown line

-- #1b
parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg tree@(Node l x r) =
    case compare <$> timestamp msg <*> timestamp x of
        Nothing -> tree
        Just LT -> Node (insert msg l) x r
        _ -> Node l x (insert msg r)
  where
    timestamp :: LogMessage -> Maybe Int
    timestamp (LogMessage _ t _) = Just t
    timestamp _ = Nothing

-- #3
build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = helper []
  where
    helper :: [LogMessage] -> MessageTree -> [LogMessage]
    helper acc Leaf = acc
    helper acc (Node l x r) = helper acc l ++ [x] ++ helper acc r

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap message . inOrder . build . filter relevant
  where
    message :: LogMessage -> String
    message (LogMessage _ _ msg) = msg
    message (Unknown msg) = msg

    relevant :: LogMessage -> Bool
    relevant (LogMessage (Error n) _ _) | n >= 50 = True
    relevant _ = False
