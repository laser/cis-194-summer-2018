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
import Data.Char
import Data.Maybe
import Data.List

-- #1
ex1 :: a -> b -> b
ex1 int bool = bool

-- #2
ex2 :: a -> a -> a
ex2 int int2 = int2

-- #3
ex3 :: Int -> a -> a
ex3 int a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 bool a a2 = a

-- #5
ex5 :: Bool -> Bool
ex5 bool = bool

-- #6
ex6 :: (a -> a) -> a
ex6 = undefined

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f a = a

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
ex10 :: Maybe a -> a
ex10 = fromJust


-- #11
ex11 :: a -> Maybe a
ex11 = return

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ val Leaf = Node Leaf val Leaf
insertBST comp val node@(Node left x right)
  | compVal == GT = insertBST comp val right
  | compVal == LT = insertBST comp val left
  | compVal == EQ = node
    where compVal = comp val x

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps (x:xs) =
  case safeHead x of
    Just h -> case isUpper h of
      True -> allCaps xs
      False -> False
    Nothing -> False


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace str = reverse $ dropWhile (\a -> a == ' ') $ reverse str

-- #16
firstLetters :: [String] -> [Char]
firstLetters strings = catMaybes $ map safeHead $ strings

-- #17
asList :: [String] -> String
asList strings = "[" ++ concat (intersperse "," ( filter (not . null) strings)) ++ "]"
