
module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen

import Homework.Week07.Sized
import Homework.Week07.JoinList
import Homework.Week07.Scrabble
import Homework.Week07.Buffer

import Data.Monoid
import Data.Foldable (fold)

main :: IO ()
main = hspec spec

instance Arbitrary Size where
    arbitrary = sizeGen

sizeGen :: Gen Size
sizeGen = pure $ Size 1

instance ( Arbitrary m
         , Arbitrary a
         , Monoid m ) => Arbitrary (JoinList m a) where
    arbitrary = sized joinListGen

joinListGen :: (Arbitrary m, Arbitrary a, Monoid m) 
            => Int -> Gen (JoinList m a)
joinListGen depth 
    | depth == 0 = Single <$> arbitrary <*> arbitrary
    | depth > 0 = 
        oneof
            [ Single <$> arbitrary <*> arbitrary
            , (+++) <$> (joinListGen (depth - 1)) <*> (joinListGen (depth - 1))]

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

genSafeStrings :: Gen String
genSafeStrings = fmap foldStrings $ listOf genSafeString

genScopedInt :: (Int -> Bool) -> Gen Int
genScopedInt f = suchThat (arbitrary :: Gen Int) f

foldStrings :: [String] -> String
foldStrings = 
    let f a b = if a /= "" then a ++ "\n" ++ b else b
    in foldr f ""

-- Naive tree traversal size count.
sizeJ :: JoinList m a -> Int
sizeJ Empty = 0
sizeJ (Single _ _) = 1
sizeJ (Append _ l r) = sizeJ l + sizeJ r

spec :: Spec
spec = do
  describe "tag" $ do
    it "gets the annotation at the root of a JoinList" $ do
      --pending
      tag (Append (Sum 5) (Single (Sum 3) 'a') (Single (Sum 2) 'b')) `shouldBe` Sum 5
      tag (Single (Sum 10) 'a') `shouldBe` Sum 10

  describe "+++" $ do
    it "appended joinlist should always `mappend` child annotations" $ do
      property $
          \jl1 jl2 -> 
              let m1 = (getSize . size . tag) (jl1 :: JoinList Size Char)
                  m2 = (getSize . size . tag) (jl2 :: JoinList Size Char)
                  m3 = (getSize . size . tag) $ (+++) jl1 jl2
              in m1 + m2 == m3
    it "appends two JoinList structures together" $ do
      --pending
      let a = Single (Sum 3) 'a'
      let b = Single (Sum 2) 'b'
      (+++) a b `shouldBe` Append (Sum 5) a b

  describe "exercise 2" $ do
    let jl = Append (Size 3) (Append (Size 2) (Single (Size 1) 's') (Single (Size 1) 'u')) (Single (Size 1) 'p')

    describe "quickCheck" $ do
      it "indexJ should always behave the same as ((!!?) . jlToList)" $ do
        --pending
        property $ 
          \jl i -> 
            (==) (indexJ i jl) 
                 ((!!?) (jlToList (jl :: JoinList Size Char)) (i :: Int))

      it "dropJ should always behave the same as (drop i $ jlToList)" $ do
        --pending
        property $ 
          \jl i -> 
            (==) (jlToList $ dropJ i jl) 
                 (drop (i :: Int) (jlToList (jl :: JoinList Size Char)))

      it "takeJ should always behave the same as (take i $ jlToList)" $ do
        --pending
        property $ 
          \jl i -> 
            (==) (jlToList $ takeJ i jl) 
                 (take (i :: Int) (jlToList (jl :: JoinList Size Char)))

    describe "indexJ" $ do
      it "finds a letter at the specified index" $ do
        --pending
        indexJ 0 jl `shouldBe` Just 's'
        indexJ 1 jl `shouldBe` Just 'u'
        indexJ 2 jl `shouldBe` Just 'p'

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
          (Single (Score 10, Size 1) "z")
          :: JoinList (Score, Size) String

    describe "quickCheck" $ do
      it "`toString . fromString` is `identity :: String -> String`" $ do
        --pending
        property $ 
          forAll genSafeStrings $
          \xs -> 
            let identity ys = toString (fromString ys :: JoinList (Score, Size) String)
            in identity xs == xs

      it "`fromString . toString` is `identity :: JoinList m a -> JoinList m a`" $ do
        --pending
        property $ 
          forAll genSafeStrings $
          \xs -> 
            let jl = fromString xs :: JoinList (Score, Size) String
                identity jl' = fromString (toString jl') :: JoinList (Score, Size) String
            in identity jl == jl

      it "numLines gives same result as length after filtering non-newlines" $ do
        --pending
        property $ 
          forAll genSafeStrings $
          \xs -> 
            let jl = fromString xs :: JoinList (Score, Size) String
            in (numLines jl) == (length . filter (\x -> x == '\n')) xs

      it "replaceLine adjusts score appropriately" $ do
        --pending
        property $ 
          forAll (suchThat genSafeStrings (\x -> x /= "")) $ \xs -> 
          forAll genSafeString $ \str -> 
            let jl = fromString xs :: JoinList (Score, Size) String
                sizeJl = numLines jl
                pred   = \x -> (x >= 0 && x < sizeJl) || (x == 0 && sizeJl == 0)
            in forAll (genScopedInt pred) $ \i ->
              let (Score s)  = scoreString . fold $ (line i jl)
                  jl2        = replaceLine i str jl
                  (Score s') = scoreString str
              in value jl2 == (value jl - s) + s'

      it "`line i` after `replaceLine i str` returns `str`" $ do
        --pending
        property $ 
          forAll (suchThat genSafeStrings (\x -> x /= "")) $ \xs -> 
          forAll genSafeString $ \str -> 
            let jl     = fromString xs :: JoinList (Score, Size) String
                sizeJl = numLines jl
                pred   = \x -> (x >= 0 && x < sizeJl) || (x == 0 && sizeJl == 0)
            in forAll (genScopedInt pred) $ \i ->
              let jl2 = replaceLine i str jl
              in (fold $ line i jl2) == str

      it "`replaceLine i str` is idempotent" $ do
        --pending
        property $ 
          forAll genSafeStrings $ \xs -> 
          forAll genSafeString $ \str -> 
            let jl = fromString xs :: JoinList (Score, Size) String
                sizeJl = numLines jl
            in forAll (genScopedInt (\x -> (x >= 0 && x < sizeJl) || x == 0 && sizeJl == 0)) $ \i ->
              let jl2 = replaceLine i str jl
                  jl3 = replaceLine i str jl2
              in jl2 == jl3

      it "replaceLine with a new line then replaceLine with the original line \
         \ gives the original JoinList" $ do
        --pending
        property $ 
          forAll genSafeStrings $ \xs -> 
          forAll genSafeString $ \str -> 
            let jl = fromString xs :: JoinList (Score, Size) String
                sizeJl = numLines jl
            in forAll (genScopedInt (\x -> (x >= 0 && x < sizeJl) || x == 0 && sizeJl == 0)) $ \i ->
              let jl2 = replaceLine i str jl
                  str' = fold $ line i jl
                  jl3 = replaceLine i str' jl2
              in jl3 == jl

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
