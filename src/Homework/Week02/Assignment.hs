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

import Data.Monoid ((<>))
import Data.Maybe (Maybe(..), catMaybes)

-- #1a
parseMessage :: String -> LogMessage
parseMessage m =
  case words m of
    ("E":code:timestamp:rest) -> LogMessage (Error (read code)) (read timestamp) (unwords rest)
    ("I":timestamp:rest) -> LogMessage Info (read timestamp) (unwords rest)
    a -> Unknown (unwords a)

-- #1b
parse :: String -> [LogMessage]
parse =  map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert m t = case m of
  Unknown _ -> t
  LogMessage mt ts rest -> insert' mt ts rest t

insert' :: MessageType -> TimeStamp -> String -> MessageTree -> MessageTree
insert' mt ts rest t =
  case t of
    Leaf -> Node Leaf (LogMessage mt ts rest) Leaf
    Node l c@(LogMessage mtc tsc restc) r ->
      case compare ts tsc of
        LT -> Node (insert' mt ts rest l) c r
        EQ -> Node (insert' mtc tsc restc l) (LogMessage mt ts rest) r
        GT -> Node l c (insert' mt ts rest r)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder mt = case mt of
  Leaf -> mempty
  Node l c r -> foldl (<>) mempty [inOrder l, pure c, inOrder r]

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = catMaybes . (map errorGT50) . inOrder . build
  where
    errorGT50 :: LogMessage -> Maybe String
    errorGT50 a = case a of
      (LogMessage (Error v) ts rest) | v > 50 -> pure rest
      _ -> Nothing
