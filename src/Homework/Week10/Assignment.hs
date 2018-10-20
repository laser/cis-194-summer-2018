module Homework.Week10.Assignment where

import Test.QuickCheck
import Homework.Week10.Support (Tree(..), labelTree)

-- Exercise 1

data T a = N (T a) (T a) | L a deriving Show

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree depth
    | depth == 0 = fmap Leaf arbitrary
    | depth > 0 = 
        oneof
            [ fmap Leaf arbitrary
            , Node <$> genTree (depth - 1) <*> genTree (depth - 1)
            ]

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genTree

-- Exercise 2

size :: Tree a -> Int
size (Leaf x) = 1
size (Node l r) = (size l) + (size r)

toList :: Tree a -> [a]
toList (Leaf x) = pure x
toList (Node l r) = (toList l) ++ (toList r)

-- Exercise 3

-- The length of the list produced by toList is the size of the given tree.
prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = (length . toList) t == size t

-- labelTree does not change the size of the tree.
prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = (size . labelTree) t == size t

-- For every tree t, toList (labelTree t) is the expected list.
-- Hint: [0..n] denotes the list of numbers from 0 to n, inclusively.
prop_labelTree :: Tree Integer -> Bool
prop_labelTree t =
    (toList . labelTree) t == [0..n]
    where n = fromIntegral $ (size t) - 1

-- Applying labelTree to a list twice does yield the same list as applying it once.
prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = 
    (size . labelTree . labelTree) t == (size .  labelTree) t

main = do
    putStrLn "prop_lengthToList:"
    quickCheck prop_lengthToList
    putStrLn "prop_sizeLabelTree:"
    quickCheck prop_sizeLabelTree
    putStrLn "prop_labelTree:"
    quickCheck prop_labelTree
    putStrLn "prop_labelTreeIdempotent:"
    quickCheck prop_labelTreeIdempotent
