module Homework.Week01.Assignment where
import Data.List 
import Data.Ord

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (\n -> read [n]) (show n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft n = zipWith (*) n (cycle [1,2])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

-- #3
sumDigits :: [Integer] -> Integer
-- sumDigits n = foldl (+) 0 (concatMap toDigits n)
sumDigits = sum . concat . map toDigits


-- #4
isDivisibleByTen :: Integer -> Bool
isDivisibleByTen x = x `rem` 10 == 0

validate :: Integer -> Bool
validate = isDivisibleByTen . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

shortest :: [[a]] -> [a]
shortest [] = []
shortest ls = minimumBy (comparing length) ls

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = [ ]
hanoi4 1 a b c d = [(a, b)]
hanoi4 n a b c d = shortest [(hanoi4 (n - k) a d c b ++ hanoi k a b c ++ hanoi4 (n - k) d b a c) | k <- [1..(n-1)]]
