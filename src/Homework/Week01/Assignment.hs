module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits d ++ [r]
    where r = x `rem` 10
          d = x `div` 10


-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = r : toDigits d 
    where r = x `rem` 10
          d = x `div` 10


-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [(*2), id]) 

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum

-- #4
validate :: Integer -> Bool
validate xs
    | res == 0 = True 
    | otherwise = False
    where res = (sum xs) `rem` 10
    

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi i a b c = go (i - 1) xs
--    where go = i' ([(a, c)])
{-
hanoi 2 a b c
    go 2 [(a, c)]
    go 1 [(a, b)]
    go 0 [(c, b)]
-}

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
