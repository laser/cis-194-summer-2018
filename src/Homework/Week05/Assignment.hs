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
eval (Lit int) = int
eval (Add left right) = (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)

-- #2
evalStr :: String -> Maybe Integer
evalStr s =
  case parseExp Lit Add Mul s of 
    Nothing -> Nothing
    Just n -> Just (eval n)

-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- #4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = ( > 0)
  add = (||)
  mul = (&&)

data MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

data Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = lit ((x * y) `mod` 7)  