module Day06 where

import Utils

import Text.Megaparsec
import Text.Megaparsec.Char (space)

import Data.Map qualified as Map
import Relude.Extra.Map
import Parsing

type Fish = Int

type FishCounter = Map Fish Int

parseInput :: Parser [Fish]
parseInput = commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just fishTimers = parseMaybe parseInput input
        p1 = simulateFish fishTimers 80
        p2 = simulateFish fishTimers 256

countFish :: [Fish] -> FishCounter
countFish = Map.fromListWith (+) . (`zip` repeat 1)

simulateFish :: [Fish] -> Int -> Int
simulateFish initialFish days =
  sum . elapseNDays days . countFish $ initialFish

elapseDay :: FishCounter -> FishCounter
elapseDay counter = Map.unionWith (+) (Map.mapKeysWith (+) ageFish counter) babies
  where
    ageFish :: Fish -> Fish
    ageFish n | n == 0 = 6
              | otherwise = n - 1

    nZeros = lookupDefault 0 0 counter
    babies = Map.singleton 8 nZeros

elapseNDays :: Int -> FishCounter -> FishCounter
elapseNDays n = appEndo . stimes n . Endo $ elapseDay
