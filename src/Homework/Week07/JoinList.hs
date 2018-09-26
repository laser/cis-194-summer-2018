{-# OPTIONS_GHC -Wall -Werror #-}
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

import Data.Monoid ((<>))
import Homework.Week07.Buffer
import Homework.Week07.Scrabble
import Homework.Week07.Sized

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x1 x2 = Append (tag x1 <> tag x2) x1 x2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Append m l r)
    | getSize (size m) <= n = Nothing
    | Size n < size (tag l) = indexJ n l
    | otherwise             = indexJ (n - getSize (size (tag l))) r
indexJ 0 (Single _ a) = Just a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n xs | n <= 0 = xs
dropJ n (Append m l r)
    | getSize (size m) <= n = Empty
    | Size n < size (tag l) = dropJ n l +++ r
    | otherwise             = dropJ (n - (getSize $ size $ tag l)) r
dropJ _ _ = Empty

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
    replaceLine :: Int -> String -> JoinList (Score, Size) String ->
                   JoinList (Score, Size) String
    replaceLine = undefined

    -- | Compute the number of lines in the buffer.
    numLines :: JoinList (Score, Size) String -> Int
    numLines = undefined

    -- | Compute the value of the buffer, i.e. the amount someone would
    --   be paid for publishing the contents of the buffer.
    value :: JoinList (Score, Size) String -> Int
    value = undefined
