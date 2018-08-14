module Homework.Week03Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week03.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "skips" $ do
    it "outputs the input list if empty" $ do
      skips ([] :: [Int]) `shouldBe` []

    it "outputs in the nth list every nth element from the input list" $ do
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

  describe "localMaxima" $ do
    it "returns all the local maxima in the input list, in order" $ do
      localMaxima [1, 2] `shouldBe` []
      localMaxima [1, 2, 3] `shouldBe` []
      localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
      localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      localMaxima [1, 2, 3, 4, 5] `shouldBe` []

  describe "histogram" $ do
    it "takes as input a list of Integers between 0 and 9 (inclusive) and \
        \ outputs a vertical histogram showing how many of each number were in \
        \ the input list" $ do

      let hs1 = "\
                 \ *        \n\
                 \ *        \n\
                 \ *   *    \n\
                 \==========\n\
                 \0123456789\n"

      histogram [1, 1, 1, 5] `shouldBe` hs1

      let hs2 = "\
                 \    *     \n\
                 \    *     \n\
                 \    * *   \n\
                 \ ******  *\n\
                 \==========\n\
                 \0123456789\n"

      histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] `shouldBe` hs2
