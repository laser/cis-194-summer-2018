module Homework.Week01.Assignment where

import Data.Monoid

-- #1a
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | x > 0 = (toDigits (div x 10)) <> [mod x 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | x > 0 = [mod x 10] <> (toDigitsRev (div x 10))

-- #
doubleEveryOther = reverse . doubleEveryOther' . reverse

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:rest) = [x, 2*y] <> doubleEveryOther' rest

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0 . (=<<) toDigits

-- #4
validate :: Integer -> Bool
validate = (==) 0 . ((flip mod) 10) . checksum

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther. toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi i source dest temp = hanoi (i-1) source temp dest <> [(source, dest)] <> hanoi (i-1) temp dest source

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
