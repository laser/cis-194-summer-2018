module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = map (\x -> read x ::Integer) $ map (\x -> [x]) $ show n

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
{-toDigitsRev = reverse $ toDigits-}

-- #2
doubleEveryOther :: [Int] -> [Int]
{-doubleEveryOther :: [Integer] -> [Intger]-}
doubleEveryOther foo = [ x  * (1 + (i `mod` 2)) | (x,i) <- zip foo [0..length foo]]

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- #4
validate :: Integer -> Bool
validate = undefined

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
