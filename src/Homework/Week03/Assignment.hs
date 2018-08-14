module Homework.Week03.Assignment (
  --skips,
  localMaxima,
  histogram
) where

import Data.List

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips x = x : skips' 2 x

skips' :: Int -> [a] -> [[a]]
skips' 0 x = [x]
skips' _ [] = []
skips' n x = if n > length x then [] else (takeEnd $ noEnds $ splitAt' n x) : (skips' (n + 1) x) 

splitAt' :: Int -> [a] -> [[a]]
splitAt' 0 _ = []
splitAt' _ [] = []
splitAt' n x = if n > length x then [] else (fst (splitAt n x)) : (splitAt' n (snd (splitAt n x)))

noEnds :: [[a]] -> [[a]]
noEnds [] = []
noEnds x = [a | a <- x, length a >= (length (head x))]

takeEnd :: [[a]] -> [a]
takeEnd [] = []
takeEnd (x:xs) = last x : (takeEnd xs)

-- #2
-- 173 characters 
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima a@(l:m:r:t) = if m > l && m > r then m : (localMaxima $ drop 1 a) else localMaxima $ drop 1 a

-- 148 characters 
{-
localMaxima :: [Integer] -> [Integer]
localMaxima a@(l:m:r:t)
  | length a < 3 = []
  | otherwise = if m > l && m > r then m : (localMaxima $ drop 1 a) else localMaxima $ drop 1 a
-}
 
-- #3
isInLine :: [Integer] -> [Bool]
isInLine [] = []
isInLine x = [elem c x | c <- [0..9]]

whiteOrStar :: Bool -> Char
whiteOrStar x = if x then '*' else ' '

getLinez :: [Integer] -> String
getLinez x = map whiteOrStar (isInLine x)
-- Above works fine!

getAllLinez :: [Integer] -> [String]
getAllLinez [] = []
getAllLinez x = map getLinez (transpose . group . sort $ x)


histogram :: [Integer] -> String
histogram [] = []
histogram x = (intercalate "\n" $ reverse $ getAllLinez x) ++ "\n==========\n0123456789\n"


test :: [Integer] -> String
test [] = []
test x = histogram [1,4,5,4,6,6,3,4,2,4,9]









