module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map ($ xs) $ (map takeEvery [1..(length xs)])
  where takeEvery _ [] = []
        takeEvery n xs  | n >= length xs = []
                        | otherwise = head (drop n xs) : takeEvery n (tail $ drop n xs)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, y, _) -> y) $ filter (\(x, y, z) -> y > x && y > z) $ (zip3 xs (drop 1 xs) (drop 2 xs))

-- #3
histogram :: [Integer] -> String
histogram = undefined
