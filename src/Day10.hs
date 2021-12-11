module Day0X where

import Utils (showSolutions)
import Text.Megaparsec
import Parsing

parseInput :: Parser ()
parseInput = pure ()

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just parsed = parseMaybe parseInput input
        p1 = part1 parsed
        p2 = part2 parsed

errorScore :: Text -> Int
errorScore line = undefined
  where Left (ParseErrorBundle errs _) = parse chunk "a line" line

chunk :: Parser ()
chunk = asumMap try 
  [ between '(' ')' (many chunk)
  , between '[' ']' (many chunk)
  , between '{' '}' (many chunk)
  , between '<' '>' (many chunk)
  ]


part1 :: () -> ()
part1 = undefined

part2 :: () -> ()
part2 = undefined
