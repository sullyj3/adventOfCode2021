module Day07 where

import Data.List (maximum, minimum, (!!))
import Data.Semigroup (Arg (Arg))
import Parsing (Parser, commaSeparatedInts)
import Text.Megaparsec (MonadParsec (eof), parseMaybe)
import Text.Megaparsec.Char (space)
import Utils (showSolutions)
import Linear.V2 (V2(..))

parseInput :: Parser [Int]
parseInput = commaSeparatedInts <* space <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just crabs = parseMaybe parseInput input

    V2 p1 p2 = sum [ V2 (movementCostp1 crab)
                        (movementCostp2 crab) | crab <- crabs ]

    movementCostp1 crab =       abs $ crab - targetp1
    movementCostp2 crab = tri . abs $ crab - targetp2

    targetp1 = median crabs
    targetp2 = mean crabs

median :: Ord a => [a] -> a
median ns = sort ns !! (length ns `div` 2)

mean :: [Int] -> Int
mean ns = sum ns `div` length ns

-- >>> map tri [0..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
tri :: Int -> Int
tri n = n * (n + 1) `div` 2
