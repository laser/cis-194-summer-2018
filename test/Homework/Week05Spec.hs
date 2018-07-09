module Homework.Week05Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week05.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 5 specs" $
    it "are commented out below" pending
{-
  describe "eval" $ do
    it "evaluates Lit expressions as plain integers" $ do
      property $ \x -> eval (Lit x) `shouldBe` x

    it "evaluates Add expressions as addition" $ do
      property $ \x y -> eval (Add (Lit x) (Lit y)) `shouldBe` x + y

    it "evaluates Mul expressions as addition" $ do
      property $ \x y -> eval (Mul (Lit x) (Lit y)) `shouldBe` x * y

    it "evaluates nested arithmetic expression ASTs" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "evaluates invalid arithmetic expression strings to Nothing" $ do
      evalStr "2+3*" `shouldBe` Nothing

    it "evaluates valid arithmetic expression strings to integers" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "2+3*4" `shouldBe` Just 14

  describe "Expr ExprT" $ do
    it "produces expression ASTs" $ do
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe`
        Mul (Add (Lit 2) (Lit 3)) (Lit 4)

  describe "Expr Integer" $ do
    it "evaluates arithmetic expressions to integers" $ do
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` (20 :: Integer)

  describe "Expr Bool" $ do
    it "interprets negative values as False" $ do
      property $ \(Positive x) -> lit (-x) `shouldBe` False

    it "interprets zero as False" $ do
      lit 0 `shouldBe` False

    it "interprets positive values as True" $ do
      property $ \(Positive x) -> lit x `shouldBe` True

    it "evaluates addition as logical or" $ do
      add (lit 0) (lit 0) `shouldBe` False
      add (lit 1) (lit 0) `shouldBe` True
      add (lit 0) (lit 1) `shouldBe` True
      add (lit 1) (lit 1) `shouldBe` True

    it "evaluates multiplication as logical and" $ do
      mul (lit 0) (lit 0) `shouldBe` False
      mul (lit 1) (lit 0) `shouldBe` False
      mul (lit 0) (lit 1) `shouldBe` False
      mul (lit 1) (lit 1) `shouldBe` True

    it "evaluates arithmetic expressions as logical operations" $ do
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` True
      mul (add (lit 2) (lit 3)) (lit 0) `shouldBe` False

  describe "Expr MinMax" $ do
    it "wraps literals in the MinMax newtype" $ do
      property $ \x -> lit x `shouldBe` MinMax x

    it "evaluates addition as maximum" $ do
      property $ \x y -> add (lit x) (lit y) `shouldBe` MinMax (max x y)

    it "evaluates multiplication as minimum" $ do
      property $ \x y -> mul (lit x) (lit y) `shouldBe` MinMax (min x y)

    it "evaluates arithmetic expressions as minimums/maximums" $ do
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` MinMax 3

  describe "Expr Mod7" $ do
    it "produces mod 7 literals" $ do
      property $ \x -> lit x `shouldBe` Mod7 (x `mod` 7)

    it "evaluates addition mod 7" $ do
      property $ \x y -> add (lit x) (lit y) `shouldBe` Mod7 ((x + y) `mod` 7)

    it "evaluates multiplication mod 7" $ do
      property $ \x y -> mul (lit x) (lit y) `shouldBe` Mod7 ((x * y) `mod` 7)

    it "evaluates arithmetic expressions to integers mod 7" $ do
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` Mod7 6
-}
