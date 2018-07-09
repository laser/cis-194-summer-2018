module Homework.Week06Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week06.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "returns fibonacci numbers" $ do
      pending
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 3 `shouldBe` 2
      fib 4 `shouldBe` 3
      fib 5 `shouldBe` 5
      fib 6 `shouldBe` 8
      fib 7 `shouldBe` 13

  describe "fibs1" $ do
    it "is a list of the fibonacci numbers" $ do
      pending
      take 8 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13]

  describe "fibs2" $ do
    it "is a list of the fibonacci numbers" $ do
      pending
      take 8 fibs2 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13]

  describe "streamRepeat" $ do
    it "creates a stream containing only the provided element" $ do
      pending
      take 8 (streamToList $ streamRepeat 5)
        `shouldBe` ([5, 5, 5, 5, 5, 5, 5, 5] :: [Integer])

  describe "streamMap" $ do
    it "produces a new stream of the applied function's results" $ do
      pending
      take 8 (streamToList $ streamMap (+1) (streamRepeat 5))
        `shouldBe` ([6, 6, 6, 6, 6, 6, 6, 6] :: [Integer])

  describe "streamFromSeed" $ do
    it "produces a stream by unfolding the seed with the given function" $ do
      pending
      take 8 (streamToList $ streamFromSeed (+1) 0)
        `shouldBe` ([0, 1, 2, 3, 4, 5, 6, 7] :: [Integer])
      take 8 (streamToList $ streamFromSeed (++ "~") "")
        `shouldBe` ["", "~", "~~", "~~~", "~~~~", "~~~~~", "~~~~~~", "~~~~~~~"]

  describe "nats" $ do
    it "is a stream of the natural numbers" $ do
      pending
      take 8 (streamToList nats) `shouldBe` [0, 1, 2, 3, 4, 5, 6, 7]

  describe "ruler" $ do
    it "is a stream that corresponds to the ruler function" $ do
      pending
      take 16 (streamToList ruler)
        `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]
