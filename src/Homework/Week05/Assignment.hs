--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import qualified Homework.Week05.StackVM as S

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

-- 5)

instance Expr S.Program where
    lit:: Integer -> S.Program
    lit x = [S.PushI x]
    add :: S.Program -> S.Program -> S.Program
    add x y = y ++ x ++ [S.Add]
    mul x y = y ++ x ++ [S.Mul]
    
compile :: String -> Maybe S.Program
compile = parseExp lit add mul
