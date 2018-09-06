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
-- #1
ex1 :: a -> b -> b
ex1 a b = b

-- #2
ex2 :: a -> a -> a
ex2 a b = a

-- #3
ex3 :: Int -> a -> a
ex3 i a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 bool a a' = a

-- #5
ex5 :: Bool -> Bool
ex5 a = a

-- #6
ex6 :: (a -> a) -> a
ex6 f = let x = f x in x

-- #7
ex7 :: (a -> a) -> a -> a
ex7 = id

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = fmap

-- #10
ex10 :: Maybe a -> a
ex10 (Just a) = a

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 a = a

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f a Leaf = Node (Leaf) a (Leaf)
insertBST f a (Node l b r) =
    case f a b of
        GT -> Node l b (insertBST f a r)
        otherwise -> Node (insertBST f a l) b r
        

-- #14
allCaps :: [String] -> Bool
allCaps xs = all (\x -> x == True) $ map allCaps' xs

allCaps' :: String -> Bool
allCaps' [] = False
allCaps' (x:xs) = toUpper x == x

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace [] = []
dropTrailingWhitespace xs = go (reverse xs) 
    where go (y:ys) =
            case y == ' ' of
                    True -> go ys
                    False -> reverse (y:ys)

-- #16
firstLetters :: [String] -> [Char]
firstLetters = f . g
    where f = foldr (++) "" 
          g = sequence . filter (\x -> x /= Nothing) . map safeHead

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

-- #17
asList :: [String] -> String
asList = filter (\x -> x /= '"') . show . filter (\x -> x /= "")
