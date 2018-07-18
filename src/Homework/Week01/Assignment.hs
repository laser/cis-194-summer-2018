module Homework.Week01.Assignment where

import Data.Char
import Data.List

-- #1a
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | otherwise = map (toInteger . digitToInt) . show $ x

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id, (*2)]) . reverse

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = foldl' (+) 0 . concatMap toDigits

-- #4
validate :: Integer -> Bool
validate x = sum (concatMap toDigits $ doubleEveryOther $ toDigits x) `rem` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
