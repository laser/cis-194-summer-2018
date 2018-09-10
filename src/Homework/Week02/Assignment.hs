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
parseMessage x = case words x of
  ("E":sev:ts:msg) -> LogMessage (Error (read sev)) (read ts) (unwords msg)
  ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
  ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
  _ -> Unknown x

-- #1b
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)


-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) Leaf = Leaf
insert x Leaf = Node (Leaf) x (Leaf)
insert (LogMessage x y z) (Node left (LogMessage t ts str) right)
  | y < ts = Node (insert (LogMessage x y z) left) (LogMessage t ts str) right
  | y > ts = Node left (LogMessage t ts str) (insert (LogMessage x y z) right)


-- #3
build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] mt = mt
build' (lm:lms) mt = build' lms (insert lm mt)

build :: [LogMessage] -> MessageTree
build lms = build' lms Leaf


-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

-- #5
greaterThan50 :: [LogMessage] -> [LogMessage]
greaterThan50 [] = []
greaterThan50 (lm@(LogMessage (Error sev) _ _):lms)
  | sev >= 50 = lm : (greaterThan50 lms)
  | otherwise = greaterThan50 lms
greaterThan50 (_:lms) = greaterThan50 lms

justMessages :: [LogMessage] -> [String]
justMessages [] = []
justMessages (LogMessage _ _ msg:lms) = msg : justMessages lms
justMessages (_:lms) = justMessages lms

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = justMessages (inOrder (build (greaterThan50 lms)))
