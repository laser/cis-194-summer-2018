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
import Data.Function
import Data.Char
import Data.List
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
ex4 x y z = y

-- ex4 :: Bool -> a -> a -> a
-- ex4 x y z = z

-- #5
-- Your answer must include information on how many distinct functions inhabit this type.
ex5 :: Bool -> Bool
ex5 x = x 

-- ex5 :: Bool -> Bool
-- ex5 x = not x

-- #6
ex6 :: (a -> a) -> a
ex6 = fix

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x 

-- can't 7 just ignore the function and return a?

-- #8
ex8 :: [a] -> [a]
ex8 [] = []
ex8 xs = xs 

-- #9
-- this is map 
ex9 :: (a -> b) -> [a] -> [b]
ex9 f [] = []
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
ex12 (Nothing) = Nothing
ex12 (Just x) = Just x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f new (Leaf) = Node Leaf new Leaf
insertBST f new (Node leftTree value rightTree)
  | f new value == GT = Node leftTree value (insertBST f new rightTree)
  | otherwise = Node (insertBST f new leftTree) value rightTree

order :: Ord a => a -> a -> Ordering 
order x y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

-- #14
allCaps :: [String] -> Bool
allCaps x = foldl isCapital True x 

isCapital :: Bool -> String -> Bool
isCapital _ [] = False
isCapital False _ = False
isCapital True (x:xs) 
  | isUpper x = True
  | otherwise = False

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhiteSpace . reverse 

dropWhiteSpace :: String -> String
dropWhiteSpace [] = []
dropWhiteSpace (x:xs) = if x == ' ' then dropWhiteSpace xs else (x:xs)

-- #16
firstLetters :: [String] -> [Char]
firstLetters = (convert . (map firstLetter))

convert :: [Maybe Char] -> [Char]
convert [] = []
convert (Nothing:xs) = []
convert ((Just x):xs) = x:(convert xs)

firstLetter :: String -> Maybe Char
firstLetter "" = Nothing
firstLetter (x:xs) = Just x

{--
asList :: [String] -> String
Examples:
asList ["alpha","beta","gamma"] == "[alpha,beta,gamma]"
asList []                       == "[]"
asList ["lonely"]               == "[lonely]"
--}

-- #17
asList :: [String] -> String
asList x = "[" ++ (intercalate "," x) ++ "]"


