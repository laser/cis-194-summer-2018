module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Control.Arrow
import Data.Maybe
import Data.List (sort, group, transpose)
import Data.Function
import qualified Data.Map.Strict as M (Map, fromList, lookup)

-- #1
skips :: [a] -> [[a]]
skips xs = (skips' xs) <$> [1..(length xs)]
  where
    pattern n xs = cycle((take (n-1) (repeat (\_ -> Nothing))) ++ [Just])
    skips' :: [a] -> Int -> [a]
    skips' xs n = mapMaybe (\(a, b) -> a b) (zip (pattern n xs) xs)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = zip3 (drop 2 xs) (drop 1 xs) xs
  & filter (\(a, b, c) -> b > a && b > c)
  & map (\(_, b, _) -> b)

-- #3
histogram :: [Integer] -> String
histogram =  histogram'
  >>> bars
  >>> transpose
  >>> reverse
  >>> append ["=========="]
  >>> append ["0123456789"]
  >>> unlines
  where
    bars :: M.Map Int Int -> [String]
    bars hist = map (bar (maximum hist) . fromMaybe 0 . (flip M.lookup) hist ) [0..9]
    bar :: Int -> Int -> String
    bar max n =  (take n $ repeat '*') ++ (take (max - n) $ repeat ' ')
    histogram' :: [Integer] -> M.Map Int Int
    histogram' = M.fromList . map (\a -> (fromInteger $ head a, length a)) . group . sort
    append = flip (++)
