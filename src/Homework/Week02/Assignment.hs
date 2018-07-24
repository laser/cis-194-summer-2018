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
import Data.Monoid
import Data.Char
import Data.List (dropWhileEnd)

-- #1a

stripl :: String -> String
stripl [] = []
stripl (x:xs) = case x == ' ' of
    True -> stripl xs
    False -> (x:xs)

stripr = dropWhileEnd isSpace

parseInt :: String -> (Maybe Int, String)
parseInt [] = (Nothing, "")
parseInt str = go str "" 
    where go [] "" = (Nothing, "")
          go [] res = (Just (read res :: Int), "")
          go (' ':xs) res = (Just (read res :: Int), xs)
          go (x:xs) res = case isDigit x of
                            True -> go xs (res ++ [x])
                            False -> (Nothing, res)

parseMessage :: String -> LogMessage
parseMessage (x:xs) = case x of
    'E' -> case parseInt $ stripl xs of
            (Nothing, str) -> Unknown (x:xs)
            (Just x', str) -> 
                case parseInt $ stripl str of
                    (Nothing, str) -> Unknown (x:xs)
                    (Just y', str) -> LogMessage (Error x') y' $ stripr str
    'I' -> case parseInt $ stripl xs of
            (Nothing, str) -> Unknown (x:xs)
            (Just x', str) -> LogMessage Info x' $ stripr str
    'W' -> case parseInt $ stripl xs of
            (Nothing, str) -> Unknown (x:xs)
            (Just x', str) -> LogMessage Warning x' $ stripr str
    otherwise -> Unknown (x:xs)


--I 6 Completed armadillo processing

-- #1b

split s = 
    case dropWhile (\x -> x == '\n') s of
        "" -> []
        s' -> w : split s''
            where (w, s'') = break (\x -> x == '\n') s'
          
parse :: String -> [LogMessage]
parse str = map parseMessage $ split str

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown str) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert (LogMessage mt dt str) (Node left (LogMessage mt' dt' str') right) = 
    case dt < dt' of
        True -> Node (new_node left) node right
        False -> Node left node $ new_node right
    where node = (LogMessage mt' dt' str')
          new_node = insert (LogMessage mt dt str)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (\b a -> insert a b) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg Leaf) = [msg]
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map f . filter g . inOrder . build
    where g (LogMessage (Error severity) _ _) = severity >= 50
          g _ = False
          f (LogMessage _ _ msg) = msg
