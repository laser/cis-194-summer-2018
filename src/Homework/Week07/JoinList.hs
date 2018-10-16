{-# LANGUAGE FlexibleInstances #-}

module Homework.Week07.JoinList (
  tag,
  indexJ,
  (+++),
  (!!?),
  jlToList,
  dropJ,
  takeJ,
  scoreLine,
  Sized(..),
  JoinList(..)
) where

import Homework.Week07.Buffer
import Homework.Week07.Scrabble
import Homework.Week07.Sized
import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m     => JoinList m a -> m
tag Empty           = mempty
tag (Single m a)    = m
tag (Append m x x') = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) (Single m a) (Single m' a') = Append (m <> m') (Single m a) (Single m' a')
(+++) (Append m l r) (Append m' l' r') = Append (m <> m') (Append m l r) (Append m' l' r')

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single m a)   = [a]
jlToList (Append m l r) = jlToList l <> jlToList r

(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:xs)  !!? 0    = Just x
(x:xs)  !!? i    = xs !!? (i-1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Append m l r)
    | getSize (size m) <= i = Nothing
    | Size i < size (tag l) = indexJ i l
    | otherwise = indexJ (i - getSize (size (tag l))) r
indexJ i (Single m a) = if i == (getSize $ size m) then (Just a) else Nothing
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i (Append m l r)
    | getSize (size m) <= i = Empty
    | Size i < size (tag l) = dropJ n l +++ r
    | otherwise = indexJ (i - getSize (size (tag l))) r
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i (Append m l r)
    | getSize (size m) == i = (Append m l r)
    | getSize (size m) < i = Empty
    | getSize (size m) > i = takeJ (i - getSize (size (tag r))) l +++ r
takeJ i (Single m a)
    | getSize (size m) == i = Single m a
    | otherwise = Empty

scoreLine :: String -> JoinList Score String
scoreLine = undefined

instance Buffer (JoinList (Score, Size) String) where
  fromString = undefined
  line = undefined
  numLines = undefined
  replaceLine = undefined
  toString = undefined
  value = undefined
