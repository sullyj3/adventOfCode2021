module DayX where

import Utils (showSolutions)
import Text.Megaparsec
import Parsing

parseInput :: Parser ()
parseInput = pure ()

solve :: Text -> Text
solve input = showSolutions () ()
  where Just parsed = parseMaybe parseInput input
        -- p1 = part1 parsed
        -- p2 = part2 parsed

-- part1 :: () -> ()
-- part1 = undefined

-- part2 :: () -> ()
-- part2 = undefined
