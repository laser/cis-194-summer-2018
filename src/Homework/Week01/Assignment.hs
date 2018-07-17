module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = toDigits d ++ [r]
    where r = x `rem` 10
          d = x `div` 10


-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x  
    | x <= 0 = []
    | otherwise = r : toDigitsRev d
    where r = x `rem` 10
          d = x `div` 10


-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ zipWith ($) (cycle [id, (*2)]) $ reverse xs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (fmap toDigits)

-- #4
validate :: Integer -> Bool
validate x
    | f x == 0 = True 
    | otherwise = False
    where f = flip rem 10 . (sum . concat . fmap toDigits . doubleEveryOther . toDigits)

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src dest aux = []
hanoi i src dest aux = 
    (hanoi (i-1) src aux dest) ++ [(src, dest)] ++ (hanoi (i-1) aux dest src)

{-
hanoi 3 a b c =
    [(a, b)]
    [(a, b), (a, c)]
    [(a, b), (a, c), (b, c)]
    [(a, b), (a, c), (b, c), (a, b)]
    [(a, b), (a, c), (b, c), (a, b), (c, a)]
    [(a, b), (a, c), (b, c), (a, b), (c, a), (c, b)]
    [(a, b), (a, c), (b, c), (a, b), (c, a), (c, b), (a, b)]

h 3 a b c
(h 2 a c b) ++ [(a, b)] ++ (h 2 c b a)
(h 1 a b c) ++ [(a, c]) ++ (h 1 b c a) ++ [(a, b)] ++ (h 1 c a b) ++ [(c, b)] ++ (h 1 a b c)
[(a, b)] ++ [(a, c)] ++ [(b, c)] ++ [(a, b)] ++ [(c, a)] ++ [(c, b)] ++ [(a, b)]

-}

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
--hanoi4 0 _ _ _ = []
--hanoi4 i src dest aux aux' = (hanoi (i - 2) src aux dest

h 1 src dest aux1 aux2 = [(src, dest)]
h i src dest aux1 aux2 = 
    (h (i - 2) src aux1 dest aux2) ++ (h (i - 2) src aux2 aux1 dest) ++ [(src, dest)] ++ (h (i - 2) aux1 dest src aux2) ++ (h (i - 2) aux2 dest aux1 src)
