module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- rev :: [Integer] -> [Integer]
-- rev [] = []
-- rev [x] = [x]
-- rev (x:xs) = rev xs ++ [x]

-- toNumber :: [Integer] -> Integer
-- toNumber [] = -1
-- toNumber [x] = x
-- toNumber (x:xs) = x + toNumber (multByTen xs)


multByTen :: [Integer] -> [Integer]
multByTen [] = []
multByTen [x] = [x * 10]
multByTen (x:xs) = x * 10 : multByTen xs

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 1 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

-- toDigitsRev 5
-- 5 `mod` 10 : toDigitsRev (5 `div` 10)
-- 5 : toDigitsRev (0)
-- 5 : []
-- [5]

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x,y] = [x*2, y]
doubleEveryOther [x,y,z] = [x, y*2, z]
doubleEveryOther (x:y:xs) = x*2 : y : (doubleEveryOther xs)

-- doubleEveryOther [2,3,4,5]
-- 2 * 2 : 3 : doubleEveryOther [4, 5]
-- 4 : 3 : [4*2, 5]
-- [4 , 3 , 8, 5]

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | toDigitsRev x == [x] = x
  | otherwise = sumDigits (toDigitsRev x)
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

-- sumDigits :: [Integer] -> Integer
-- sumDigits = sum . concat map toDigits

-- #4
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
