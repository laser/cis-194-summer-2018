module Homework.Week03.Assignment (

  skips,
  localMaxima,
  histogram
) where


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f b [] = b
foldl' f b (x:xs) = foldl' f (f b x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)

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
histogram xs = go (histo xs) [] $ maximum $ histo xs 
    where i = maximum xs
          go _ ys 0 = join '\n' (ys++["==========\n0123456789\n"])
          go xs ys i = 
            let (ints, str) = histoRow xs
            in go ints (str:ys) (i-1)

histo :: [Integer] -> [Integer]
histo xs = go xs [] 0
    where go _ zs 10 = zs
          go ys zs i = go ys (zs++[fromIntegral $ length $ filter (\x -> x == i) ys]) $ i+1

join char [] = ""
join char (w:ws) = w ++ go ws
  where
    go [] = ""
    go (v:vs) = char : (v ++ go vs)

histoRow :: [Integer] -> ([Integer], String)
histoRow xs = (map (\x -> if x == 0 then 0 else x-1) xs,
               map (\x -> if x == 0 then ' ' else '*') xs) 
