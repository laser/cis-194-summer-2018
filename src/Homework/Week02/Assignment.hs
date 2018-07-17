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
