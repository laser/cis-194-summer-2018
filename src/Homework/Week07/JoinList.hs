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
tag Empty           = mempty
tag (Single m a)    = m
tag (Append m x x') = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) (Single m a) (Single m' a') = Append (m <> m') (Single m a) (Single m' a')
(+++) (Append m l r) (Append m' l' r') = Append (m <> m') (Append m l r) (Append m' l' r')
(+++) (Append m l r) (Single m' a) = Append (m <> m') (Append m l r) (Single m' a)
(+++) (Single m' a) (Append m l r) = Append (m <> m') (Single m' a) (Append m l r)

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single m a)   = [a]
jlToList (Append m l r) = jlToList l <> jlToList r

(!!?) :: [a] -> Int -> Maybe a
[] !!? _         = Nothing
_  !!? i | i < 0 = Nothing
(x:xs)  !!? 0    = Just x
(x:xs)  !!? i    = xs !!? (i-1)

gss :: Sized a => a -> Int
gss = getSize . size

gsst :: (Monoid b, Sized b) => JoinList b a -> Int
gsst = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single m a) = Just a
indexJ i (Append m l r)
    | i < gsst l = indexJ i l
    | otherwise = indexJ (i - gsst l) r
indexJ _ _ = Nothing


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ _ Empty = Empty
dropJ i (Single m a)
    | i >= getSize (size m) = Empty
    | otherwise = Single m a
dropJ i (Append m l r)
    | i >= gss m = Empty
    | i == gsst l = r
    | otherwise = (dropJ i l) +++ r


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i (Append m l r)
    | i >= gss m = Append m l r
    | i == gsst l = l 
    | i > gsst l = l +++ takeJ (i - gsst l) r
    | otherwise = takeJ i l
takeJ i (Single m a)
    | i >= gss m = Single m a
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
