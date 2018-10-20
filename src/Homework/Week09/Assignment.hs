module Homework.Week09.Assignment (
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr,
  Ident(..),
  Atom(..),
  SExpr(..)
) where

import Control.Applicative
import Data.Char

import Homework.Week09.AParser

newtype P a = P { runParser' :: String -> Maybe (a, String) }

-- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = many    
    where many = some <|> pure []
          some = (:) <$> p <*> many
    
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = some
    where many = some <|> pure []
          some = (:) <$> p <*> many

-- #2
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = do
    a <- satisfy isAlpha
    b <- zeroOrMore $ satisfy isAlphaNum
    return $ a:b

-- #3
type Ident = String

data Atom = N Integer
          | I Ident
  deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)

parseAtom :: Parser Atom
parseAtom = do
    atom <- (fmap I ident) <|> (fmap N posInt)
    spaces
    return atom

parseA :: Parser SExpr
parseA = do
    a <- fmap A parseAtom
    spaces
    return a

parseComb :: Parser SExpr
parseComb = do
    char '('
    comb <- fmap Comb (zeroOrMore parseSExpr)
    char ')'
    spaces
    return comb

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseA <|> parseComb
