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
first f (a, b) = (f a, b) 

first' :: (a -> b) -> (c,a) -> (c,b)
first' = fmap 

instance Functor Parser where
  fmap f pa = Parser $ \s -> do
    (a, xs)  <- runParser pa $ s
    return (f a, xs)
  fmap f xs = xs >>= return . f

-- #2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  pab <*> pa = Parser $ \s -> do
    (a, xs) <- runParser pa $ s
    (ab', ys) <- runParser pab $ xs
    return (ab' a, ys)

instance Monad Parser where
    return = pure
    (>>=) p f = Parser $ \s -> do
        (a, xs) <- runParser p s
        (b, ys) <- runParser (f a) xs
        return (b, ys)

        
-- #3
safeHead :: [a] -> Maybe (a, [a])
safeHead [] = Nothing
safeHead (x:xs) = Just (x, xs)

abParser :: Parser (Char, Char)
abParser = do
    a <- char 'a'
    b <- char 'b'
    return (a, b)

abParser_ :: Parser ()
abParser_ = do
    a <- char 'a'
    b <- char 'b'
    return ()

intPair :: Parser [Integer]
intPair = do
    a <- posInt
    char ' '
    b <- posInt
    return [a, b]

-- #4

failure :: Parser a
failure = Parser $ \s -> Nothing

combine :: Parser a -> Parser a -> Parser a
combine pa pb = Parser $ \s ->
    case runParser pa $ s of
        Just (a, xs) -> Just (a, xs)
        Nothing -> case runParser pb $ s of
            Just (b, ys) -> Just (b, ys)
            Nothing -> Nothing

instance Alternative Parser where
  empty = failure
  a <|> b = combine a b

-- #5
parseUpper :: Parser ()
parseUpper = do
    satisfy isUpper
    return ()

parseInt :: Parser ()
parseInt = do
    posInt
    return ()

intOrUppercase :: Parser ()
intOrUppercase = parseUpper <|> parseInt
