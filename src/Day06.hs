module Day06 where


import Text.Megaparsec
import Text.Megaparsec.Char (space)

import Data.IntMultiSet qualified as IMS
import Data.IntMultiSet (IntMultiSet)

import Parsing
import Data.List ((!!))
import Utils

type Fish = Int
type FishCounter = IntMultiSet

parseInput :: Parser [Fish]
parseInput = commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just fishTimers = parseMaybe parseInput input
        p1 = IMS.size $ iterate elapseDay initialCounts !! 80
        p2 = IMS.size $ iterate elapseDay initialCounts !! 256

        initialCounts :: FishCounter
        initialCounts = IMS.fromList fishTimers

elapseDay :: FishCounter -> FishCounter
elapseDay = IMS.concatMap \case
  0 -> [6,8]
  n -> [n-1]
