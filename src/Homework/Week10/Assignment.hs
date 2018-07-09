module Homework.Week10.Assignment where

import Homework.Week10.Support (Tree(..), labelTree)

-- Exercise 1

-- instance Arbitrary a => Arbitrary (Tree a) where

-- Exercise 2

size :: Tree a -> Int
size = undefined

toList :: Tree a -> [a]
toList = undefined

-- Exercise 3

-- The length of the list produced by toList is the size of the given tree.
prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList = undefined

-- labelTree does not change the size of the tree.
prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree = undefined

-- For every tree t, toList (labelTree t) is the expected list.
-- Hint: [0..n] denotes the list of numbers from 0 to n, inclusively.
prop_labelTree :: Tree Integer -> Bool
prop_labelTree = undefined

-- Applying labelTree to a list twice does yield the same list as applying it once.
prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent = undefined
