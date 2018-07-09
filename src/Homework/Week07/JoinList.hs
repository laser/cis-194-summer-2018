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

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag = undefined

(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList = undefined

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = undefined

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = undefined

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
