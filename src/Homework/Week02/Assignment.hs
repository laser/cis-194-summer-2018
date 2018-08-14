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
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
