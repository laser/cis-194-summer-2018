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
fib  0 = 0
fib  1 = 1
fib  2 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib_closed n = floor (((phi ^ n) / (sqrt 5)) + 0.5)
    where phi = (1 + (sqrt 5)) / 2

-- #2
fibs2 :: [Integer]
fibs2 =  map fib_closed [0..]

fibs3 = [0,1] ++ [fibs3 !! (n- 1) + fibs3 !! (n-2) | n <- [2..]]
fibs4 = 0 : 1 : zipWith (+) fibs4 (tail fibs4)
    

-- #3
{-data Stream a = Stream a -- replace this with your own definition; this one is wrong-}
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = [x] ++ (streamToList xs)
streamToList2 (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
   show x = show (take 20 $ streamToList x)
{-instance Show a => Show (Stream a) where-}
   {-show x = show .take 20 . streamToList -}

-- #4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = undefined
{-streamFromSeed f (Cons n0 xs) = Cons n1 (streamFromSeed f (Cons n1 xs))-}
 {-where n1 = f n0-}

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined