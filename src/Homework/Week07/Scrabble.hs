module Homework.Week07.Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Show, Eq)

instance Monoid Score where
  mempty = Score 0
  mappend (Score a) (Score b) = Score $ a + b

scoreKey :: [(Char, Score)]
scoreKey =
    let chars  = ['A'..'Z']
        scores = fmap Score [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
    in zip chars scores
           
score :: Char -> Score
score char = f (toUpper char) scoreKey
    where f _ [] = mempty
          f c (x:xs) = if c == fst x then snd x else f c xs

scoreString :: String -> Score
scoreString = foldMap score
