{-# OPTIONS_GHC -Wall -Werror #-}

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

import Data.Char (isSpace, isUpper)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Homework.Week04.BST

-- #1
ex1 :: a -> b -> b
ex1 = \_ b -> b
-- one implementation

-- #2
ex2 :: a -> a -> a
ex2 = \_ a -> a
-- two implementations
--   \a _ -> a
--   \_ a -> a

-- #3
ex3 :: Int -> a -> a
ex3 = \_ a -> a
-- one implementation

-- #4
ex4 :: Bool -> a -> a -> a
ex4 = \_ _ a -> a
-- four implementations
--   \_ _ a -> a
--   \_ a _ -> a
--   \p a1 a2 -> if p then a1 else a2
--   \p a1 a2 -> if p then a2 else a1

-- #5
ex5 :: Bool -> Bool
ex5 = id
-- four implementations
--   \x -> x
--   \x -> not x
--   \_ -> True
--   \_ -> False

-- #6
ex6 :: (a -> a) -> a
ex6 = undefined
-- no legal implementations

-- #7
ex7 :: (a -> a) -> a -> a
ex7 = \_ x -> x
-- a family of implementations, one for each n <- [0..]
--   \f x -> foldr (.) id $ replicate n f $ x

-- #8
ex8 :: [a] -> [a]
ex8 = id
-- a bajillion implementations, here are a few families for n <- [0..]
--   \xs -> []
--   \xs -> drop n xs
--   \xs -> take n xs
--   \xs -> if null xs then [] else replicate n xs
--   \xs -> f xs ++ g xs (where f, g are taken from above)

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 _ [] = []
ex9 f (x:xs) = f x : ex9 f xs
-- this, with precomposition or postcomposition with anything from ex8

-- #10
ex10 :: Maybe a -> a
ex10 = undefined
-- no legal implementations

-- #11
ex11 :: a -> Maybe a
ex11 = \_ -> Nothing
-- two implementations
--   \_ -> Nothing
--   \x -> Just x

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = \_ -> Nothing
-- two implementations
--   \_ -> Nothing
--   \x -> x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST cmpr x (Node l c r) =
    case cmpr x c of
        LT -> Node (insertBST cmpr x l) c r
        _ -> Node l c (insertBST cmpr x r)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- #14
allCaps :: [String] -> Bool
allCaps = all $ maybe False isUpper . safeHead

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

-- #17
asList :: [String] -> String
asList strs = "[" ++ intercalate "," (filter (not <$> null) strs) ++ "]"
