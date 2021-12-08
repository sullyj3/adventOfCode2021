module Day08 where

import Utils
import Motif
import qualified Data.Text as T

type Entry = ([String], [String])

solve :: Text -> Text
solve input = showSolutions p1 p2
  where entries = map parseEntry . T.lines $ input
        p1 = part1 entries
        p2 = part2 ()

parseEntry :: Text -> Entry
parseEntry = error "not implemented"

part1 :: [Entry] -> Int
part1 = count (`elem` [1,4,7,8]) . concatMap determineOutput

determineOutput :: Entry -> [Int]
determineOutput = error "not implemented"

part2 :: () -> ()
part2 = id
