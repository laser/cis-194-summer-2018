{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser
import Control.Applicative
import Prelude


-- #1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2

-- #2
evalStr :: String -> Maybe Integer
evalStr = fmap eval <$> parseExp Lit Add Mul

-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


-- #4
instance Expr Integer where
  lit a = eval $ Lit a
  add a b = a + b
  mul a b = a * b

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord, Num)

instance Expr MinMax where
  lit a = MinMax a
  add = max
  mul = min

instance Expr Mod7 where
  lit a = Mod7 $ a `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

