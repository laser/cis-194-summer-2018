module Homework.Week04Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week04.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "insertBST" $ do
    it "properly handles inserts into an empty tree" $ do
      pending
      let emptyTree = (Leaf :: BST Char)
      insertBST (\_ _ -> EQ) 'x' emptyTree `shouldBe` (Node Leaf 'x' Leaf)

  describe "allCaps" $ do
    it "checks to see if a list of strings contains only capitalized words" $ do
      pending
      allCaps ["Hi", "There"] `shouldBe` True
      allCaps ["HI", "THERE"] `shouldBe` True
      allCaps [] `shouldBe` True
      allCaps ["", "Blah"] `shouldBe` False
      allCaps ["Hi", "there"] `shouldBe` False

  describe "dropTrailingWhitespace" $ do
    it "drops the trailing whitespace from a string" $ do
      pending
      dropTrailingWhitespace "foo" `shouldBe` "foo"
      dropTrailingWhitespace "" `shouldBe` ""
      dropTrailingWhitespace "bar    " `shouldBe` "bar"
      dropTrailingWhitespace "bar  baz " `shouldBe` "bar  baz"

  describe "firstLetters" $ do
    it "gets the first letter, if exits, from a list of strings" $ do
      pending
      firstLetters ["foo", "bar"] `shouldBe` ['f', 'b']
      firstLetters ["alpha", ""] `shouldBe` ['a']
      firstLetters [] `shouldBe` []
      firstLetters ["", ""] `shouldBe` []

  describe "asList" $ do
    it "renders a bracketed list given a list of strings" $ do
      pending
      asList ["alpha", "beta", "gamma"] `shouldBe` "[alpha,beta,gamma]"
      asList [] `shouldBe` "[]"
      asList ["lonely"] `shouldBe` "[lonely]"
      asList ["up", "", "down"] `shouldBe` "[up,down]"

