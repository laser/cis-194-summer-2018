module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips [x] = [x] : []
skips x = x : []
skips all@(x:xs) = (x : []) : [] 


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
histogram :: [Integer] -> String
histogram [] = "\n==========\n0123456789\n"

test :: [Integer] -> IO()
test x = putStr "*\n**\n==========\n0123456789\n"
test x = putStr $ histogram x
