module Day07 where

import Utils (showSolutions)
import Text.Megaparsec
import Parsing
import Text.Megaparsec.Char (space)
import Data.List ((!!), minimum, maximum)
import Data.Semigroup (Arg (Arg))

parseInput :: Parser [Int]
parseInput = commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just crabs = parseMaybe parseInput input
        p1 = part1 crabs
        p2 = part2 crabs

median :: Ord a => [a] -> a
median ns = sort ns !! (length ns `div` 2)

part1 :: [Int] -> Int
part1 crabs = sum [abs (crab-med) | crab <- crabs]
  where med = median crabs

-- >>> map tri [0..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
tri :: Int -> Int
tri n = n * (n+1) `div` 2

-- >>> argMin (\x -> x * x) [-10..10]
-- 0
-- >>> argMin id [-10..10]
-- -10
argMin :: Ord b => (a -> b) -> [a] -> a
argMin f candidates = theMin
  where Arg _ theMin = minimum [ Arg (f x) x | x <- candidates]

-- >>> part2 [16,1,2,0,4,2,7,1,2,14]
part2 :: [Int] -> Int
part2 crabs = totalFuelCost
  where 
    totalFuelCost = sum [movementCost target crab | crab <- crabs]
    (left, right) = (minimum crabs, maximum crabs)
    target = argMin (\pos -> sum [movementCost pos crab | crab <- crabs]) [left..right]
    movementCost pos crab = tri (abs $ pos - crab)

