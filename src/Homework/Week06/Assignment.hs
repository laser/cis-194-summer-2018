{-# OPTIONS_GHC -Wall -Werror -fno-warn-name-shadowing #-}

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
  Stream(..),
  zero,
  one,
  num,
  act,
  x,
  fibs3,
  fib4,
  Matrix(..)
) where

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- #2
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

-- #3
data Stream a = a ::: Stream a
infixr 5 :::

streamToList :: Stream a -> [a]
streamToList (x ::: xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = x ::: streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x ::: xs) = f x ::: streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x ::: streamFromSeed f (f x)

streamDrop :: Int -> Stream a -> Stream a
streamDrop n xs
    | n <= 0    = xs
    | otherwise = let _ ::: xs' = xs in streamDrop (n - 1) xs'

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = rulerN 2

rulerN :: Integer -> Stream Integer
rulerN n = streamMap (powers n) $ streamDrop 1 nats

powers :: Integer -> Integer -> Integer
powers p = helper 0 where
    helper i n =
        case divMod n p of
            (n', 0) -> helper (i + 1) n'
            _       -> i

zero :: Num a => Stream a
zero = streamRepeat 0

one :: Num a => Stream a
one = 1 ::: zero

num :: Num a => a -> Stream a
num a = a `act` one

act :: Num a => a -> (Stream a -> Stream a)
act a as = streamMap (a*) as

x :: Num a => Stream a
x = 0 ::: num 1

instance Num a => Num (Stream a) where
    (a:::as) + (b:::bs) = a + b ::: as + bs
    (a:::as) * (b:::bs) = a * b ::: act a bs + act b as + (0 ::: as * bs)
    negate (a:::as)     = negate a ::: negate as
    fromInteger n       = num (fromInteger n)
    abs                 = id
    signum              = const 1

instance Fractional a => Fractional (Stream a) where
    fromRational n = num (fromRational n)
    (a:::as) / (b:::bs) = let q = a / b ::: act (1 / b) (as - q * bs) in q

fibs3 :: Stream Integer
fibs3 = streamMap floor $ (x :: Stream Rational) / (1 - x - x * x)

data Matrix =
    Matrix
        { e11 :: !Integer
        , e12 :: !Integer
        , e21 :: !Integer
        , e22 :: !Integer
        }

instance Num Matrix where
    (Matrix a11 a12
            a21 a22) * (Matrix b11 b12
                               b21 b22) =
        Matrix (a11*b11 + a12*b21) (a11*b12 + a12*b22)
               (a21*b11 + a22*b21) (a21*b12 + a22*b22)

    (Matrix a11 a12
            a21 a22) + (Matrix b11 b12
                               b21 b22) =
        Matrix (a11 + b11) (a12 + b12)
               (a21 + b21) (a22 + b22)

    negate m =
        Matrix (negate $ e11 m) (negate $ e12 m)
               (negate $ e21 m) (negate $ e22 m)

    fromInteger n = Matrix n 0 0 n
    abs = id
    signum = const 1

fib4 :: Integer -> Integer
fib4 n = e12 $ (Matrix 1 1 1 0)^n
