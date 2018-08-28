module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Homework.Week04.BST

-- #1
ex1 :: a -> b -> b
ex1 x y = y

-- #2
ex2 :: a -> a -> a
ex2 x y = y  

-- #3
ex3 :: Int -> a -> a
ex3 x y = y 

-- #4
-- Your answer must include information on how many distinct functions inhabit this type.
ex4 :: Bool -> a -> a -> a
ex4 x y z = x 

-- #5
-- Your answer must include information on how many distinct functions inhabit this type.
ex5 :: Bool -> Bool
ex5 x = x 

-- #6
ex6 :: (a -> a) -> a
ex6 = error "Impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x 

-- #8
ex8 :: [a] -> [a]
ex8 [] = []
ex8 xs = xs 

-- #9
-- this is map 
ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] _ = []
ex9 f (x:xs) = (f x) : (ex9 f xs) 

-- #10
ex10 :: Maybe a -> a
ex10 Nothing = error "Impossible"
ex10 (Just x) = x

-- #11
ex11 :: a -> Maybe a
ex11 x = Just x

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST = undefined

-- #14
allCaps :: [String] -> Bool
allCaps = undefined

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = undefined

-- #16
firstLetters :: [String] -> [Char]
firstLetters = undefined

-- #17
asList :: [String] -> String
asList = undefined
