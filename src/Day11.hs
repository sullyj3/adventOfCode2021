module Day11 where

import Utils (showSolutions)
import Data.Text qualified as T
import Data.Char (digitToInt)
import Flow
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable (MVector)

type EnergyLevel = Int
type Grid = Vector (Vector EnergyLevel)
type MGrid s = MVector s (MVector s EnergyLevel)

-- todo maybe vector? decide after reading problem
parseInput :: Text -> Grid
parseInput = T.lines .> map T.unpack .> map (map digitToInt) .>
  map V.fromList .> V.fromList

solve :: Text -> Text
solve input = showSolutions octopi p2
  where octopi = parseInput input
        p1 = ()
        p2 = ()

part1 :: () -> ()
part1 = undefined

part2 :: () -> ()
part2 = undefined
