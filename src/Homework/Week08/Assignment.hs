{-# LANGUAGE InstanceSigs #-}

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
first f (x,y) = (f x, y) 

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = answer
            where 
              answer = Parser $ step1
              --step1 :: ((->) String (Maybe (b, String))) Bad type annotation!
              step1 = step2 g $ runParser pa  
              step2 :: (a -> b) -> ((->) String (Maybe (a, String))) -> ((->) String (Maybe (b, String)))
              step2 f r = fmap (step1a2 f) r 
              step1a2 :: (a -> b) -> Maybe (a, String) -> Maybe (b, String)
              step1a2 f m = fmap (first f) m 
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
