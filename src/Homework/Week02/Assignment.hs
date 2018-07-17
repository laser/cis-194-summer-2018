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
parseMessage line = fold (words line) LogMessage
  where fold ("I":xs) msgCtr = fold' xs (msgCtr Info)
        fold ("E":code:xs) msgCtr = fold' xs (msgCtr (Error (readCode code)))
        fold rest _ = Unknown (unwords rest)
        fold' (timestamp:xs) msgCtr = fold'' xs (msgCtr (readCode timestamp))
        fold'' rest msgCtr = msgCtr (unwords rest)
        readCode code = read code::Int

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
