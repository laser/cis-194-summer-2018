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

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m x x') = m


(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty = []
jlToList (Single m a) = [a]
jlToList (Append m l r) = jlToList l <> jlToList r

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) (Single m a) (Single m' a') = Append (m <> m') (Single m a) (Single m' a')
(+++) (Append m l r) (Append m' l' r') = Append (m <> m') (Append m l r) (Append m' l' r')

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = undefined

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = undefined

scoreLine :: String -> JoinList Score String
scoreLine = undefined

instance Buffer (JoinList (Score, Size) String) where
  fromString = undefined
  line = undefined
  numLines = undefined
  replaceLine = undefined
  toString = undefined
  value = undefined
