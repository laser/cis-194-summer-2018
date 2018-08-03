module Homework.Week06.Assignment (
  fib,
  fibs1,
  fibs2,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  nats,
  ruler,
  Stream(..)
) where

-- #1a
fib :: Integer -> Integer
fib n = f 0 1 0
    where f n1 n2 i = if i == n then n1 else f n2 (n1+n2) (i+1)

fibs1 :: [Integer]
fibs1 = [ fib n | n <- [0..]]

-- #2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- #3
data Stream a = Cons a (Stream a) deriving (Show)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a:(streamToList b)

--instance Show a => Show (Stream a) where
--   show = undefined

-- #4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f (b))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

{-

1 2 1 3 1  2  1  4  1  3  1  3
2 4 6 8 10 12 14 16 18 20 22 24

p 0 1 0 2 0 1 0 3 0 1  0  2  0  1  0  4  0  1  0  2  0  1  0  3
n 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

-}

log2 :: Integer -> Integer
log2 = truncate . logBase 2 . fromInteger

powersOfTwo :: [Integer]
powersOfTwo = [2^x | x <- [1..]]

maxPower :: Integer -> Integer
maxPower x = log2 . last . filter (\y  -> x `mod` y == 0) $ takeWhile (<= x) $ powersOfTwo

evens :: Stream Integer
evens = streamMap maxPower (streamFromSeed (+2) 2)

odds :: Stream Integer
odds = streamRepeat 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x y) (Cons x' y') =
    Cons x (Cons x' (interleaveStreams y y'))

ruler :: Stream Integer
ruler = interleaveStreams odds evens
