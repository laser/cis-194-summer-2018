module Homework.Week03.Assignment (
  skips,
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
skips' n x = if n > length x then [] else (z $ q $ s n x) : (skips' (n + 1) x) 

-- takes an int and a list and returns a list of lists
-- the int specifies the length of the sublists
-- the [a] is the list to be divided
s :: Int -> [a] -> [[a]]
s 0 _ = []
s _ [] = []
s n x = if n > length x then [] else (fst (splitAt n x)) : (s n (snd (splitAt n x)))

-- Takes a list of lists and returns a list of lists
q :: [[a]] -> [[a]]
q [] = [] -- empty list returns an empty list
q x = [a | a <- x, length a >= (length (head x))] -- return list of lists that are as long as the head of the list passed in

-- Takes a list of lists and returns the last element of each list in the list
z :: [[a]] -> [a]
z [] = [] -- empty list returns empty list
z (x:xs) = last x : (z xs) -- get last element of the first list and cons it to the last elements of the rest of the lists. Recursive 



-- #2
-- takes a list of integers and returns a list of integers
-- 173 characters 
localMaxima :: [Integer] -> [Integer]
localMaxima [] = [] -- if empty list return empty list
localMaxima [_] = [] -- if singleton list return empty list
localMaxima [_,_] = [] -- if two element list return empty list
-- if middle of first three elements is greater than the elements to it's right and left, then cons it with the localMaxima of the tail of the whole list a. Else, forget the middle of the first three elements and take the localMaxima of the tail of the whole list 
localMaxima a@(l:m:r:t) = if m > l && m > r then m : (localMaxima $ tail a) else localMaxima $ tail a

-- 148 characters 
{-
localMaxima :: [Integer] -> [Integer]
localMaxima a@(l:m:r:t)
  | length a < 3 = []
  | otherwise = if m > l && m > r then m : (localMaxima $ drop 1 a) else localMaxima $ drop 1 a
-}
 
-- #3
-- Takes a list of numbers and return a list of bool
isInLine :: [Integer] -> [Bool]
isInLine [] = [] -- if empty list than return empty list
isInLine x = [elem c x | c <- [0..9]] -- elem each number in c, from 0 - 9, with numbers in x. If c exist in x, true, else, false

-- takes a bool and returns a char
spaceOrStar :: Bool -> Char
spaceOrStar x = if x then '*' else ' ' -- if true, return an asterisk, if false, return a whitespace character

-- takes a list of Integers and returns a String 
getLinez :: [Integer] -> String
getLinez x = map spaceOrStar (isInLine x) -- map spaceOrStar (Bool -> Char) function over the list that is returned from isInLine x (Integer -> Bool)

getAllLinez :: [Integer] -> [String]
getAllLinez [] = [] -- empty list returns empty list
getAllLinez x = map getLinez (transpose . group . sort $ x)
-- (transpose . group . sort) takes x, and sorts it, then groups it, then transposes it to turn the each sublist into a row.
-- The first element in the resulting list is the bottom row of the histogram. The second element in the list of integer lists is the second row of the histogram, etc.
-- then map getLinez over each Integer list in the result of (transpose . group . sort) and create a list of strings. Each string in the list is a representation of a row in the histogram now.

histogram :: [Integer] -> String
histogram [] = []
histogram x = (intercalate "\n" $ reverse $ getAllLinez x) ++ "\n==========\n0123456789\n"
-- getAllLinez x and then take that list and prepare it to be appended to a String. To do that first reverse the list of rows so the first row is on 
-- the rightmost side ( so it will be printed first).
-- Second, intercalate "\n" characters so each string in the list can fill its' own row


test :: [Integer] -> String
test [] = []
test x = histogram [1,4,5,4,6,6,3,4,2,4,9]









