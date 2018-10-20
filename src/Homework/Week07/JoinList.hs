{-# LANGUAGE InstanceSigs #-}
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
    | i >= gss m = Empty
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
    | gss m == i = Single m a
    | otherwise = Empty


toStringJ :: JoinList (Score, Size) String -> String
toStringJ Empty = ""
toStringJ (Single m a) = a
toStringJ (Append m l r) = toStringJ l <> "\n" <> toStringJ r

insertJ :: String -> JoinList (Score, Size) String
insertJ str = f str Empty
    where f [] jl = jl
          f (x:xs) Empty = f xs (Single (score x, Size 1) [x])
          f (x:xs) jl =  jl +++ (f xs (Single (score x, Size 1) [x]))

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

getScore :: Score -> Int
getScore (Score x) = x

replaceLineJ :: Int 
             -> String 
             -> JoinList (Score, Size) String 
             -> JoinList (Score, Size) String
replaceLineJ i str (Append m l r)
    | i < gsst l = replaceLineJ i str l +++ r
    | otherwise = l +++ replaceLineJ (i - gsst l) str r
replaceLineJ i str (Single m a)
    | i == gss m = Single (scoreString str, Size 1) str
    | i == 0 = Single (scoreString str, Size 1) str
    | otherwise = Empty
replaceLineJ _ _ Empty = Empty

swapString :: String 
           -> JoinList (Score, Size) String 
           -> JoinList (Score, Size) String
swapString str (Single m a) = Single (scoreString str, Size 1) str
swapString str jl = jl

instance Buffer (JoinList (Score, Size) String) where
  fromString :: String -> JoinList (Score, Size) String
  fromString = insertJ . filter (/= '\n')

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  numLines :: JoinList (Score, Size) String -> Int
  numLines t = case t of
    Empty -> 0
    (Single _ _) -> 1
    (Append m l r) -> gss $ snd m

  replaceLine :: Int 
              -> String 
              -> JoinList (Score, Size) String 
              -> JoinList (Score, Size) String
  replaceLine = replaceLineJ

  toString :: JoinList (Score, Size) String -> String
  toString = toStringJ

  value :: JoinList (Score, Size) String -> Int
  value t = case t of
    Empty -> 0
    (Single m _) -> getScore $ fst m
    (Append m _ _) -> getScore $ fst m
