{-# OPTIONS_GHC -Wall -Werror #-}

module Homework.Week01.Assignment where

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (=<<)

-- #1a
toDigits :: Integer -> [Integer]
toDigits xs = if xs <= 0 then [] else fmap (read . pure) $ show xs

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr step start where
    start = (cycle [id, (*2)], [])
    step x (f:fs, acc) = (fs, f x:acc)
    step _ ([], _) = error "unreachable case"

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . bind toDigits

-- #4
validate :: Integer -> Bool
validate = (0 ==) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
