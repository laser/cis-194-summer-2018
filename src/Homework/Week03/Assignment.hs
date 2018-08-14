module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List
import Data.Char

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips xs = map ($ xs) $ (map takeEvery [1..(length xs)])
  where takeEvery _ [] = []
        takeEvery n xs  | n > length xs = [] | otherwise = head (drop (n-1) xs) : takeEvery n (tail $ drop (n-1) xs)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, y, _) -> y) $ filter (\(x, y, z) -> y > x && y > z) $ (zip3 xs (drop 1 xs) (drop 2 xs))

-- #3
histogram :: [Integer] -> String
histogram xs = chart $ filter (not . empty) $ transpose $ mapToStrings $ normalize [0..9] $ map (\x -> (length x, head x)) . group . sort $ xs
  where normalize [] _ = []
        normalize (x:xs) [] = (0, x) : (normalize xs [])
        normalize (x:xs) (y:ys) | x < snd y = (0, x) : (normalize xs (y:ys))
                                | otherwise = y : (normalize xs ys)
        mapToStrings ls = map (\x -> unwords $ replicate (10 - (fst x)) " " ++ replicate (fst x) "*") ls
        empty = null . (dropWhile isSpace)
        chart ls = unlines (ls ++ ["==========", "0123456789"]) -- String