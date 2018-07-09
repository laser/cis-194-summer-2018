module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n | n < 1 = []
           | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]
  where
    lastDigit :: Integer -> Integer
    lastDigit n = mod n 10

    dropLastDigit :: Integer -> Integer
    dropLastDigit n = div n 10

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = snd $ foldr (\x (term, acc) ->
  ((if term == 2 then 1 else 2), ((x * term) : acc))) (1, []) xs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n = 0 == sumDigits (doubleEveryOther (toDigits n)) `mod` 10

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi 1 p1 p2 _  = [(p1, p2)]
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n-1) p3 p2 p1)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
