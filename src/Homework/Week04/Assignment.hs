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
import Data.List (intercalate)
import Data.Foldable (foldl)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (isUpper)
import Control.Arrow ((>>>))

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- #1
ex1 :: a -> b -> b
ex1 _ = id

-- #2
ex2 :: a -> a -> a
ex2 _ = id

-- #3
ex3 :: Int -> a -> a
ex3 _ = id

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ = id

-- #5
ex5 :: Bool -> Bool
ex5 = id

-- #6
-- No positive a ?
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 _ = id

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = fmap

-- #10
ex10 :: Maybe a -> a
-- No positive a ?
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 = pure

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ a Leaf = Node Leaf a Leaf
insertBST ord a (Node l c r) = case ord a c of
  LT -> insertBST ord a l
  EQ -> Node (insertBST ord c l) a r
  GT -> insertBST ord a r

-- #14
allCaps :: [String] -> Bool
allCaps = all isCaps
  where
    -- isCaps :: String -> Bool
    -- isCaps = safeHead >>> (fmap isUpper) >>> (fromMaybe False)
    isCaps :: String -> Bool
    isCaps = (fromMaybe False) . (fmap isUpper) . safeHead

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . (trim ' ') . reverse

trim :: Char -> [Char] -> [Char]
trim _ [] = []
trim c (x:xs) = case (x == c) of
  True -> trim c xs
  False -> (x:xs)

-- #16
firstLetters :: [String] -> [Char]
firstLetters = (map safeHead) >>> catMaybes

-- #17
asList :: [String] -> String
asList xs = "[" ++ intercalate "," (filter ((/=) "") xs) ++ "]"
