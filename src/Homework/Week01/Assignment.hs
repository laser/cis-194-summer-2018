module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits a 
    | a > 0 = toDigits (div a 10) ++ [mod a 10]
    | otherwise = []
    

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev a
    | a > 0 = (mod a 10) : toDigitsRev (div a 10)
    | otherwise = []

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:(y:ys)) = x : (y * 2) : doubleEveryOther ys

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

-- #4
validate :: Integer -> Bool
validate = undefined

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end aux = moveTopToAux ++ bottom ++ moveAuxToDest
    where   moveTopToAux = hanoi (n - 1) start aux end
            bottom = [(start, end)]
            moveAuxToDest = (hanoi (n - 1) aux end start)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d = hanoiK n [a, b, c, d]

hanoiK :: Integer -> [a] -> [(a, a)]
hanoiK 0 _ = []
hanoiK 1 (p1 : p2 : rest) = [(p1, p2)]
hanoiK n (p1 : p2 : p3 : rest) =
    hanoiK k (p1 : p3 : p2 : rest) ++
    hanoiK (n - k) (p1 : p2 : rest) ++
    hanoiK k (p3 : p2 : p1 : rest)
    where k
            | null rest   = n - 1
            | otherwise   = n `quot` 2