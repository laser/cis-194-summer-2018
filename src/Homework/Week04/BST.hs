module Homework.Week04.BST where

import Test.QuickCheck
import Control.Applicative

data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving (Eq, Show)

-- | Is the tree empty?
isEmpty :: BST a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Is the tree a BST between the given endpoints?
isBSTBetween :: (a -> a -> Ordering)    -- ^ comparison function
             -> Maybe a                 -- ^ lower bound, if one exists
             -> Maybe a                 -- ^ upper bound, if one exists
             -> BST a                   -- ^ tree to test
             -> Bool
isBSTBetween _   _       _       Leaf = True
isBSTBetween cmp m_lower m_upper (Node left x right)
  = isBSTBetween cmp m_lower  (Just x) left  &&
    isBSTBetween cmp (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower `cmp` x /= GT
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x `cmp` upper /= GT
      Nothing    -> True

-- | Is this a valid BST?
isBST :: (a -> a -> Ordering) -> BST a -> Bool
isBST cmp = isBSTBetween cmp Nothing Nothing

-- | Get a list of the elements in sorted order
getElements :: BST a -> [a]
getElements Leaf                = []
getElements (Node left x right) = getElements left ++ x : getElements right


-- TESTING CODE. (Students aren't expected to understand this yet, but it
-- might be interesting to read, anyway!)

instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized mk_tree

mk_tree :: Arbitrary a => Int -> Gen (BST a)
mk_tree 0 = return Leaf
mk_tree n = frequency [ (1, return Leaf)
                      , (2, Node <$> mk_tree (n `div` 2)
                                 <*> arbitrary
                                 <*> mk_tree (n `div` 2)) ]

prop_ordered :: BST Int -> Bool
prop_ordered x = isBST compare x == is_sorted (getElements x)
  where
    is_sorted []             = True
    is_sorted [_]            = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

test :: IO ()
test = quickCheck prop_ordered
