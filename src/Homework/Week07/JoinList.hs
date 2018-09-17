{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

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

-- Type class for data structures that can represent the text buffer
-- of an editor.
instance Buffer (JoinList (Score, Size) String) where

  -- | Convert a buffer to a String.
  toString :: JoinList (Score, Size) String -> String
  toString = undefined

  -- | Create a buffer from a String.
  fromString :: String -> JoinList (Score, Size) String
  fromString = undefined

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = undefined

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine = undefined

  -- | Compute the number of lines in the buffer.
  numLines :: JoinList (Score, Size) String -> Int
  numLines = undefined

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value :: JoinList (Score, Size) String -> Int
  value = undefined
