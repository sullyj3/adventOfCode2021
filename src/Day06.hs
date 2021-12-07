module Day06 where

import Text.Megaparsec
import Text.Megaparsec.Char (space)

import Data.IntMultiSet qualified as IMS
import Data.IntMultiSet (IntMultiSet)

import Parsing
import Utils

type FishCounter = IntMultiSet

parseInput :: Parser FishCounter
parseInput = IMS.fromList <$> commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just initialCounts = parseMaybe parseInput input
        [p1,p2] = IMS.size <$>
          selectIndices [80,256] (iterate elapseDay initialCounts)

elapseDay :: FishCounter -> FishCounter
elapseDay = IMS.concatMap \case
  0 -> [6,8]
  n -> [n-1]
