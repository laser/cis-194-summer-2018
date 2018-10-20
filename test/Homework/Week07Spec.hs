
module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week07.Sized
import Homework.Week07.JoinList
import Homework.Week07.Scrabble
import Homework.Week07.Buffer

import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tag" $ do
    it "gets the annotation at the root of a JoinList" $ do
      --pending
      tag (Append (Sum 5) (Single (Sum 3) 'a') (Single (Sum 2) 'b')) `shouldBe` Sum 5
      tag (Single (Sum 10) 'a') `shouldBe` Sum 10

  describe "+++" $ do
    it "appends two JoinList structures together" $ do
      --pending
      let a = Single (Sum 3) 'a'
      let b = Single (Sum 2) 'b'
      (+++) a b `shouldBe` Append (Sum 5) a b

  describe "exercise 2" $ do
    let jl = Append (Size 3) (Append (Size 2) (Single (Size 1) 's') (Single (Size 1) 'u')) (Single (Size 1) 'p')

    let jl2 = Append (Size 4) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')) (Append (Size 2) (Single (Size 1) 'c') (Single (Size 1) 'd')) 

    let jl3 = (Append (Size 5) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')) (Append (Size 3) (Single (Size 1) 'c') (Append (Size 2) (Single (Size 1) 'd') (Single (Size 1) 'e')) ))

    describe "indexJ" $ do
      it "finds a letter at the specified index" $ do
        --pending
        indexJ 0 jl `shouldBe` Just 's'
        indexJ 1 jl `shouldBe` Just 'u'
        indexJ 2 jl `shouldBe` Just 'p'

    describe "indexJ2" $ do
      it "finds a letter at the specified index" $ do
        --pending
        indexJ 0 jl2 `shouldBe` Just 'a'
        indexJ 1 jl2 `shouldBe` Just 'b'
        indexJ 2 jl2 `shouldBe` Just 'c'
        indexJ 3 jl2 `shouldBe` Just 'd'

    describe "indexJ3" $ do
      it "finds a letter at the specified index" $ do
        --pending
        (!!?) (jlToList jl3) 0 `shouldBe` Just 'a'
        (!!?) (jlToList jl3) 1 `shouldBe` Just 'b'
        (!!?) (jlToList jl3) 2 `shouldBe` Just 'c'
        (!!?) (jlToList jl3) 3 `shouldBe` Just 'd'
        (!!?) (jlToList jl3) 4 `shouldBe` Just 'e'

    describe "indexJ3" $ do
      it "finds a letter at the specified index" $ do
        --pending
        indexJ 0 jl3 `shouldBe` Just 'a'
        indexJ 1 jl3 `shouldBe` Just 'b'
        indexJ 2 jl3 `shouldBe` Just 'c'
        indexJ 3 jl3 `shouldBe` Just 'd'
        indexJ 4 jl3 `shouldBe` Just 'e'

    describe "dropJ" $ do
      it "returns the list when dropping 0" $ do
        --pending
        jlToList (dropJ 0 jl) `shouldBe` jlToList jl
      it "drops the first element from a JoinList" $ do
        --pending
        jlToList (dropJ 1 jl) `shouldBe` ['u', 'p']
      it "drops the first n elements from a JoinList" $ do
        --pending
        jlToList (dropJ 2 jl) `shouldBe` ['p']

    describe "takeJ" $ do
      it "returns the list when taking more than the list" $ do
        --pending
        jlToList (takeJ 4 jl) `shouldBe` jlToList jl
      it "takes the first element from a JoinList" $ do
        --pending
        jlToList (takeJ 1 jl) `shouldBe` ['s']
      it "takes the first n elements from a JoinList" $ do
        --pending
        jlToList (takeJ 2 jl) `shouldBe` ['s', 'u']
      it "takes the first n elements from a JoinList2" $ do
        --pending
        jlToList (takeJ 2 jl3) `shouldBe` ['a', 'b']

  describe "scoreLine" $ do
    it "scores a single word" $ do
      --pending
      tag (scoreLine "yay") `shouldBe` Score 9
    it "scores a with special characters" $ do
      --pending
      tag (scoreLine "haskell!") `shouldBe` Score 14
    it "scores words" $ do
      --pending
      tag (scoreLine "yay" +++ scoreLine "haskell!") `shouldBe` Score 23
    it "converts to joinLists" $ do
      --pending
      jlToList (scoreLine "yay" +++ scoreLine "haskell!") `shouldBe` ["yay", "haskell!"]

  describe "Buffer" $ do
    let testString = unlines ["a", "z"]
    let testJoinList = Append (Score 11, Size 2)
          (Single (Score 1, Size 1) "a")
          (Append (Score 10, Size 1) (Single (Score 10, Size 1) "z") Empty)
          :: JoinList (Score, Size) String
    describe "fromString" $ do
      it "converts a string to a JoinList" $ do
        --pending
        let processedList = fromString testString :: JoinList (Score, Size) String
        jlToList processedList `shouldBe` jlToList testJoinList
        tag processedList `shouldBe` (Score 11, Size 2)
    describe "toString" $ do
      it "converts a JoinList to a string" $ do
        --pending
        toString testJoinList `shouldBe` testString
    describe "line" $ do
      it "gets a single line" $ do
        --pending
        line 0 (Single (Score 2, Size 1) "a") `shouldBe` Just "a"
      it "gets a line from a joinList" $ do
        --pending
        line 0 testJoinList `shouldBe` Just "a"
      it "gets a second line from a joinList" $ do
        --pending
        line 1 testJoinList `shouldBe` Just "z"
    describe "numLines" $ do
      it "counts lines" $ do
        --pending
        numLines testJoinList `shouldBe` 2
    describe "replaceLine" $ do
      it "replaces the first line" $ do
        --pending
        let updatedList = replaceLine 0 "e" testJoinList
        jlToList updatedList `shouldBe` ["e", "z"]
        tag updatedList `shouldBe` (Score 11, Size 2)
      it "replaces another line" $ do
        --pending
        let updatedList = replaceLine 1 "e" testJoinList
        jlToList updatedList `shouldBe` ["a", "e"]
        tag updatedList `shouldBe` (Score 2, Size 2)
