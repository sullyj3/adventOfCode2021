module Day08 where

import Utils
import Motif
import qualified Data.Text as T

type Entry = ([String], [String])

solve :: Text -> Text
solve input = showSolutions p1 p2
  where entries = parseEntries input
        p1 = part1 entries
        p2 = part2 entries

parseEntry :: Text -> Entry
parseEntry input = (patterns, output) 
  where (patterns, _pipe:output) = splitAt 10 . map T.unpack . T.words $ input

parseEntries :: Text -> [Entry]
parseEntries = map parseEntry . T.lines

numSegments :: Int -> Int
numSegments = \case
  0 -> 6
  1 -> 2
  2 -> 5
  3 -> 5
  4 -> 4
  5 -> 5
  6 -> 6
  7 -> 3
  8 -> 7
  9 -> 6
  _ -> error "bad"

is147or8 :: [a] -> Bool
is147or8 = (`elem` [2, 4, 3, 7]) . length

example :: IO [Entry]
example = parseEntries <$> readFileText "inputs/day08example.txt"

part1 :: [Entry] -> Int
part1 = count is147or8 . concatMap snd

determineOutput :: Entry -> [Int]
determineOutput = error "not implemented"

part2 :: [Entry] -> Int
part2 = sum . concatMap determineOutput
