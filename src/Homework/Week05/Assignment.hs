module Homework.Week05.Assignment (
    evalStr,
    eval,
    Expr(..),
    ExprT(..),
    MinMax(..),
    Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

parseExp' = parseExp Lit Add Mul

getExp :: Maybe ExprT -> Maybe Integer
getExp (Nothing) = Nothing
getExp (Just x) =  Just (eval x) 

-- #2
evalStr :: String -> Maybe Integer
evalStr = getExp . parseExp'


-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- #4
instance Expr ExprT where
  lit x = (Lit x)
  add x y = Add x y
  mul x y = Mul x y

-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

instance Expr Integer where 
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where 
    lit x 
        | x <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y)  7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y)  7

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

-- TESTING
testExp :: Expr a => Maybe a 
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7





