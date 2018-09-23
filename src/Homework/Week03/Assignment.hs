{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE LambdaCase #-}

module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.Function
import Data.List (transpose)
import Data.Map.Strict (Map, adjust, fromAscList, toList)

skip :: Int -> [a] -> [a]
skip n xs | n < 0     = xs
          | otherwise = zip [1..] xs & filter ((0 ==) . (`mod` (n + 1)) . fst)
                                     & fmap snd

-- #1
skips :: [a] -> [[a]]
skips xs = filter (not <$> null) [ skip n xs | n <- [0..length xs] ]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs =
    zip3 xs (drop 1 xs) (drop 2 xs)
        & filter (\(l, x, r) -> x > l && x > r)
        & fmap (\(_, x, _) -> x)

-- #3
histogram :: [Integer] -> String
histogram = format . mkHist
  where
    mkHist :: [Integer] -> Map Integer Int
    mkHist = foldr (adjust (+1)) $ fromAscList [(i,0) | i <- [0..9]]

    pad :: Int -> String -> String
    pad n str =
        if length str >= n then str
        else str ++ replicate (n - length str) ' '

    format :: Map a Int -> String
    format hist = bars ++ axis ++ labels
      where
        freqs = fmap snd . toList $ hist
        max' = maximum freqs
        bars = unlines . reverse . transpose $
            fmap (\n -> pad max' ['*' | _ <- [1..n]]) freqs
        axis = "==========\n"
        labels = "0123456789\n"
