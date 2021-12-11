module Day11 where

import Utils (showSolutions)
import Text.Megaparsec
import Parsing
import Data.Text qualified as T
import Data.Char (digitToInt)
import Flow

type EnergyLevel = Int

-- todo maybe vector? decide after reading problem
parseInput :: Text -> [[EnergyLevel]]
parseInput = T.lines .> map T.unpack .> map (map digitToInt)

solve :: Text -> Text
solve input = showSolutions octopi p2
  where octopi = parseInput input
        p1 = ()
        p2 = ()

part1 :: () -> ()
part1 = undefined

part2 :: () -> ()
part2 = undefined
