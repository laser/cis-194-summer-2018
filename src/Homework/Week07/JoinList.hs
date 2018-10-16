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
tag (Single m a) = m
tag (Append m l r) = m

(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList (Append m l r) = (jlToList l) ++ (jlToList r)
jlToList (Single m a) = [a]

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (m1 `mappend` m2) a b
	where m1 = tag a
	      m2 = tag b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = undefined
{-indexJ i x | i < 0 = Nothing-}
{-indexJ i (Single m a) | i > 0 = Nothing-}
{-indexJ 0 x = Just x-}
{-indexJ i (Append m l r) = if i > lsize then (indexJ (i-lsize) r) else indexJ i l-}
		{-where lsize = getSize (tag l)-}
		      {-rsize = getSize (tag r)-}

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
