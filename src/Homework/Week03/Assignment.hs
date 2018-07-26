module Homework.Week03.Assignment (

  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips str = map (skips' str) [0..(length str - 1)]

skips' xs i = go i (drop i xs) []
    where go _ [] ys = ys
          go i' (x:xs') ys = go i' (drop i' xs') (ys++[x])

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = go xs [] 
    where go (x:y:[]) ys = ys 
          go (x:y:z:[]) ys = if y > x && y > z then (ys++[y]) else ys 
          go (x:y:z:xs) ys = if y > x && y > z
                             then go (y:z:xs) (ys++[y])
                             else go (y:z:xs) ys

-- #3
histogram :: [Integer] -> String
histogram = histoRows

histo :: [Integer] -> [Integer]
histo xs = go xs [] 0
    where go _ zs 10 = zs
          go ys zs i = go ys (zs++[fromIntegral $ length $ filter (\x -> x == i) ys]) $ i+1

res = [0,3,0,0,0,1,0,0,0,0]

join char []              =  ""
join char (w:ws)          = w ++ go ws
  where
    go []     = ""
    go (v:vs) = char : (v ++ go vs)


histoRow :: [Integer] -> ([Integer], String)
histoRow xs = (map (\x -> if x == 0 then 0 else x-1) xs,
               map (\x -> if x == 0 then ' ' else '*') xs) 

histoRows :: [Integer] -> String
histoRows xs = go (histo xs) [] $ maximum $ histo xs 
    where i = maximum xs
          go _ ys 0 = join '\n' (ys++["==========\n0123456789\n"])
          go xs ys i = 
            let (ints, str) = histoRow xs
            in go ints (str:ys) (i-1)

