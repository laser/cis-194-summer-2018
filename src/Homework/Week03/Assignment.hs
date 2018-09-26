module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = skips' [] 1 xs
everynth n xs = [x | (x,i) <- zip xs [1..], i `mod` n == 0]
skips' acc n xs 
	| n > length xs = acc
	| otherwise = skips' (acc ++ [everynth n xs]) (n+1) xs 

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima rest = localMaxima' [] rest
localMaxima' :: [Integer] -> [Integer] -> [Integer]
localMaxima' acc (x:y:z:rest) 
	| y > x && y > z = localMaxima' (acc ++ [y]) (y:z:rest)
	| otherwise = localMaxima' acc (y:z:rest)
localMaxima' acc (x:y) = acc 
localMaxima' acc x = acc 
localMaxima' acc [] = acc 

-- #3
histogram :: [Integer] -> String
histogram = undefined

-- map (\x -> (head x, length x)) . group . sort $ [1,3,1]
