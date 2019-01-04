module Homework.Week08Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week08.AParser
import Homework.Week08.Assignment

import Control.Applicative
import Data.Char (isNumber, isUpper)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Functor Parser" $ do
    describe "fmap" $ do
      it "applies a function to the result of a parser" $ do
        let p = fmap (+1) posInt
        runParser p "41" `shouldBe` Just (42, "")
        runParser p "x"  `shouldBe` Nothing

  describe "Applicative Parser" $ do
    describe "pure" $ do
      it "creates a parser that consumes nothing and returns a value" $ do
        property $ \str -> runParser (pure ()) str == Just ((), str)

    describe "<*>" $ do
      it "applies a function from a parser to the result of a parser" $ do
        let p1 = pure (+1) <*> posInt
        runParser p1 "41" `shouldBe` Just (42, "")

        let p2 = (+) <$> posInt <* char ' ' <*> posInt
        runParser p2 "12 13a" `shouldBe` Just (25, "a")

  describe "abParser" $ do
    it "parses the characters 'a' and 'b' as a pair" $ do
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
      runParser abParser "bcdefa" `shouldBe` Nothing
      runParser abParser "aecdbf" `shouldBe` Nothing

  describe "abParser_" $ do
    it "parses the characters 'a' and 'b' but returns nothing" $ do
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
      runParser abParser_ "bcdefa" `shouldBe` Nothing
      runParser abParser_ "aecdbf" `shouldBe` Nothing

  describe "intPair" $ do
    it "parses two integer values separated by a space" $ do
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

  describe "Alternative Parser" $ do
    describe "empty" $ do
      it "is a parser that always fails" $ do
        runParser (empty :: Parser ()) "abc" `shouldBe` Nothing

    describe "<|>" $ do
      it "uses the first parser if successful" $ do
        let p1 = char '*' <|> char '$'
        runParser p1 "*abc" `shouldBe` Just ('*', "abc")

        let p2 = satisfy isNumber <|> (head . show <$> posInt)
        runParser p2 "1234" `shouldBe` Just ('1', "234")

      it "user the second parser if the first one fails" $ do
        let p1 = char '*' <|> char '$'
        runParser p1 "$abc" `shouldBe` Just ('$', "abc")

        let p2 = satisfy isNumber <|> pure '*'
        runParser p2 "abcd" `shouldBe` Just ('*', "abcd")

        let p3 = satisfy isUpper <|> satisfy isNumber
        runParser p3 "1234" `shouldBe` Just ('1', "234")
        runParser p3 "*234" `shouldBe` Nothing

  describe "intOrUppercase" $ do
    it "consumes an integer or an uppercase character" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "foo" `shouldBe` Nothing
