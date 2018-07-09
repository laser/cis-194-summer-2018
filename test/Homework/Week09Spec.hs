module Homework.Week09Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week09.AParser
import Homework.Week09.Assignment

import Data.Char (isUpper)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let upper = satisfy isUpper

  describe "zeroOrMore" $ do
    it "runs the parser zero or more times" $ do
      pending
      runParser (zeroOrMore upper) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (zeroOrMore upper) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")

  describe "oneOrMore" $ do
    it "runs the parser one or more times" $ do
      pending
      runParser (oneOrMore upper) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (oneOrMore upper) "abcdeFGh" `shouldBe` Nothing

  describe "spaces" $ do
    it "parses a consecutive list of zero or more whitespace characters" $ do
      pending
      runParser spaces "   a b c " `shouldBe` Just ("   ", "a b c ")
      runParser spaces " \n \r \t a b c " `shouldBe` Just (" \n \r \t ", "a b c ")
      runParser spaces "a b c " `shouldBe` Just ("", "a b c ")
      runParser spaces "" `shouldBe` Just ("", "")

  describe "ident" $ do
    it "parses an alphabetic char followed by zero or more alphanumerics" $ do
      pending
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      runParser ident "f" `shouldBe` Just ("f", "")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing

  describe "parseSExpr" $ do
    it "parses numeric atoms" $ do
      pending
      runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")

    it "parses identifiers" $ do
      pending
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")

    it "parses lists of s-expressions" $ do
      pending
      runParser parseSExpr "(bar (foo) 3 5 874)"
        `shouldBe` Just (Comb [ A (I "bar"), Comb [A (I "foo")]
                              , A (N 3), A (N 5), A (N 874)
                              ], "")

      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
        `shouldBe` Just (Comb [ Comb [ Comb [ A (I "lambda"), A (I "x")
                                     , Comb [ A (I "lambda"), A (I "y")
                              , Comb [ A (I "plus"), A (I "x"), A (I "y") ]]]
                              , A (N 3) ], A (N 5) ], "")
