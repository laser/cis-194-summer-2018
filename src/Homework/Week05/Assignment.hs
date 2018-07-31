module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval expr = case expr of
    Lit i -> i
    Add x y -> (+) (eval x) (eval y)
    Mul x y -> (*) (eval x) (eval y)

-- #2
evalStr :: String -> Maybe Integer
evalStr xs = fmap eval (parseExp Lit Add Mul xs)

-- #3
class Expr a where
   lit :: Integer -> a 
   add :: a -> a -> a
   mul :: a -> a -> a

-- #4
instance Expr Integer where
   lit x = x
   add = (+)
   mul = (*) 

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Bool where
    lit x 
        | x <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x+y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x*y) `mod` 7
reify :: ExprT -> ExprT
reify = id
