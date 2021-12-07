module Day07 where

import Data.List (maximum, minimum, (!!))
import Data.Semigroup (Arg (Arg))
import Parsing (Parser, commaSeparatedInts)
import Text.Megaparsec (MonadParsec (eof), parseMaybe)
import Text.Megaparsec.Char (space)
import Utils (showSolutions)

parseInput :: Parser [Int]
parseInput = commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just crabs = parseMaybe parseInput input
    p1 = part1 crabs
    p2 = part2 crabs

median :: Ord a => [a] -> a
median ns = sort ns !! (length ns `div` 2)

mean :: [Int] -> Int
mean ns = sum ns `div` length ns
--
-- >>> map tri [0..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
tri :: Int -> Int
tri n = n * (n + 1) `div` 2

part1 :: [Int] -> Int
part1 crabs = sum [abs (crab - target) | crab <- crabs]
  where
    target = median crabs

part2 :: [Int] -> Int
part2 crabs = sum [movementCost target crab | crab <- crabs]
  where
    target = mean crabs
    movementCost pos crab = tri (abs $ pos - crab)


