module Homework.Week07.Scrabble where

newtype Score = Score Int
  deriving (Show, Eq)

instance Monoid Score where
  mempty = undefined
  mappend = undefined

score :: Char -> Score
score = undefined

scoreString :: String -> Score
scoreString = undefined
