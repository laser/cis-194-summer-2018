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
import Data.Char

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
  pure x = Parser $ \s -> Just (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ myFunc
            where
              -- ! These are bad type annotations ! Don't rely on type annotations so much !
              -- myFunc :: String -> Maybe (b, String) 
              myFunc s = case (runParser p1 $ s) of
                           Nothing -> Nothing
                           (Just (ab, rest)) -> secondRun (ab, rest)
              -- secondRun :: ((a -> b), String) -> Maybe (b, String)
              secondRun (f, string) = case (runParser p2 $ string) of
                                        Nothing -> Nothing
                                        (Just (v, rest)) -> Just (f v, rest)

-- #3

-- Parser (Char, Char) :: String -> ((Char, Char), String)
abParser :: Parser (Char, Char)
abParser = (,) <$> parseA <*> parseB

parseA :: Parser Char
parseA = (char 'a')

parseB :: Parser Char
parseB = (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> parseA <*> parseB

parseSpace :: Parser Char
parseSpace = (char ' ')

intPair :: Parser [Integer]
intPair = (\x _ z -> [x, z]) <$> posInt <*> parseSpace <*> posInt

-- #4
instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  p1 <|> p2 = Parser $ \s -> case runParser p1 $ s of
                              Nothing -> runParser p2 $ s
                              Just (v,rest) -> Just (v,rest)

-- #5
intOrUppercase :: Parser ()
intOrUppercase = parser1 <|> parser2

-- You can factor these two function out

parser1 :: Parser ()
parser1 = Parser $ \s -> case (runParser posInt) s of
                          Nothing -> Nothing
                          Just (v, rest) -> Just ((), rest) 

parser2 :: Parser ()
parser2 = Parser $ \s -> case (runParser (satisfy isUpper)) s of
                          Nothing -> Nothing
                          Just (v, rest) -> Just ((), rest) 