module Homework.Week02Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week02.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogAnalysis" $ do
    it "should parse error lines" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should parse info lines" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should parse unknown lines" $ do
      parseMessage "This is not in the right format" `shouldBe`  Unknown "This is not in the right format"

  describe "parse" $ do
    it "should take a multi-line string and parse out the LogMessages" $ do
      let fakeLogFile = "E 2 562 help help \nI 2 hello ma"
      parse fakeLogFile `shouldBe` [LogMessage (Error 2) 562 "help help", LogMessage Info 2 "hello ma"]

  describe "insert" $ do
    it "no-ops given an Unknown LogMessage" $ do
      insert (Unknown "foo") Leaf `shouldBe` Leaf

    it "returns a new tree with itself included, given a Leaf" $ do
      let a = Leaf
      let b = LogMessage Warning 5 "baz"
      let c = insert b a

      c `shouldBe` Node Leaf b Leaf


    it "maintains the sort order of messages in the tree" $ do
      let foo = LogMessage Warning 10 "foo"
      let baz = LogMessage Warning 5 "baz"
      let bif = LogMessage Warning 15 "bif"

      let a = Node Leaf foo Leaf
      let b = insert baz a
      let c = insert bif b

      b `shouldBe` Node (Node Leaf baz Leaf) foo Leaf
      c `shouldBe` Node (Node Leaf baz Leaf) foo (Node Leaf bif Leaf)


  describe "build" $ do
    it "builds a MessageTree from a list of LogMessages" $ do
      let foo = LogMessage Warning 10 "foo"
      let baz = LogMessage Warning 5 "baz"
      let bif = LogMessage Warning 15 "bif"

      build [foo, baz, bif] `shouldBe` Node (Node Leaf baz Leaf) foo (Node Leaf bif Leaf)


  describe "inOrder" $ do
    it "will deconstruct the MessageTree into a list of LogMessages" $ do
      let foo = LogMessage Warning 10 "foo"
      let baz = LogMessage Warning 5 "baz"
      let bif = LogMessage Warning 15 "bif"
      let gaz = LogMessage (Error 1) 20 "gaz"

      let tree = Node (Node Leaf baz Leaf) foo (Node Leaf bif (Node Leaf gaz Leaf))

      inOrder tree `shouldBe` [baz, foo, bif, gaz]


  describe "whatWentWrong" $ do
    it "will return the messages from LogMessages with Errors whose severity is 50+ - sorted by timestamp" $ do
      let messages = [LogMessage (Error 49) 10 "alpha", LogMessage (Error 51) 11 "beta", LogMessage (Error 100) 9 "kappa", Unknown "foo", LogMessage Warning 100 "blar"]

      whatWentWrong messages `shouldBe` ["kappa", "beta"]
