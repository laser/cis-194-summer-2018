module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first = undefined

instance Functor Parser where
  fmap = undefined

-- #2
instance Applicative Parser where
  pure = undefined
  _ <*> _ = undefined

-- #3
abParser :: Parser (Char, Char)
abParser = undefined

abParser_ :: Parser ()
abParser_ = undefined

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
