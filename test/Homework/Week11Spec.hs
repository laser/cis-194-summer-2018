module Homework.Week11Spec (
  main,
  spec
) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Control.Monad.Random
import Homework.Week11.Assignment

instance Arbitrary Battlefield where
  arbitrary = sized $ \size -> do
    attackerArmy <- choose (2, max size 2)
    defenderArmy <- choose (1, max size 1)
    return (Battlefield attackerArmy defenderArmy)

prop_subtractsTwoFromBattle :: Battlefield -> IO ()
prop_subtractsTwoFromBattle field = do
  newField <- evalRandIO (battle (field :: Battlefield))
  let totalUnitsLeft = attackers newField + defenders newField
  checkTotalUnitsLeft totalUnitsLeft
  where attackerArmy = attackers field
        defenderArmy = defenders field
        checkTotalUnitsLeft unitsLeft
          -- For large armies, two units will always die.
          | attackerArmy >= 3 && defenderArmy >= 2 =
            unitsLeft `shouldBe` attackerArmy + defenderArmy - 2
          -- For the minimum armies, one unit will always die.
          | attackerArmy == 2 && defenderArmy == 1 =
            unitsLeft `shouldBe` 2
          -- For battles like Battlefield 3 1, one OR two armies could die.
          | otherwise = return ()

prop_findsWinner :: Battlefield -> IO ()
prop_findsWinner field = do
  newField <- evalRandIO (invade field)
  newField `shouldSatisfy` \field ->
    case field of
      Battlefield _ 0 -> True
      Battlefield 1 _ -> True
      _ -> False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "battle" $ do
    -- prop "subtracts two units from the battle" prop_subtractsTwoFromBattle
    it "is pending" $ do
      pending

  describe "invade" $ do
    -- prop "produces a winner" prop_findsWinner
    it "is pending too" $ do
      pending

  describe "successProb" $ do
    it "finds a low probability of success" $ do
      pending
      newField <- evalRandIO (successProb (Battlefield 2 20))
      newField `shouldSatisfy` (0.1 >)

    it "finds a high probability of success" $ do
      pending
      newField <- evalRandIO (successProb (Battlefield 20 2))
      newField `shouldSatisfy` (0.9 <)
