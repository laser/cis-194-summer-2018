{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Assignment where

import System.Exit   ( exitSuccess)
import Control.Monad ( forever)
import Control.Monad.Random
import Data.Monoid
import Data.List

threeInts :: Rand StdGen Int
threeInts = do
    a <- getRandom
    return a

test :: IO Int
test = do
    r <- evalRandIO threeInts
    return r

die' :: (RandomGen g) => Rand g Int
die' = getRandomR (1,6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die')

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1, 6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

data Side = A | D deriving (Show, Eq)

-- #2 (there is no assignment #1, really)

rollDice :: Int -> Rand StdGen [Int]
rollDice = fmap (reverse . sort) . traverse (fmap unDV) . flip replicate die

score' :: (Int, Int) -> (Sum Int, Sum Int)
score' (a, d) = if a > d then (Sum 1, mempty) else (mempty, Sum 1)

scoreMap :: [Int] -> [Int] -> (Sum Int, Sum Int)
scoreMap as ds = foldMap score' $ zip as ds

availableAttackers :: Int -> Int
availableAttackers a
    | a < 2 = 0
    | a < 4 = a - 1
    | otherwise = 3

availableDefenders :: Int -> Int
availableDefenders a
    | a == 0 = 0
    | a == 1 = 1
    | otherwise = 2

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield {attackers=a, defenders=d}) = do
    aDice <- rollDice $ availableAttackers a
    dDice <- rollDice $ availableDefenders d
    let ((Sum aVictories), (Sum dVictories)) = scoreMap aDice dDice
    return $ Battlefield (a - dVictories) (d - aVictories)


-- #3

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield {attackers=a, defenders=d}) =
    if a < 2 || d <= 0
    then return b
    else do 
        result <- battle b
        invade result


-- #4
successProb :: Battlefield -> Rand StdGen Double
successProb b = f b 1000 0
    where f b 0 v = return $ (fromIntegral v) / 1000
          f b i v = do
                res <- invade b
                let a = attackers res
                let d = defenders res
                if a > d
                then f b (i - 1) (v+1)
                else f b (i - 1) v

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
