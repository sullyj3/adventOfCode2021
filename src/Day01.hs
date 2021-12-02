module Day01 where

import Motif (count)
import Utils (intList, showSolutions)
import qualified Data.Text as T

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just is = intList input
        p1 = numLt is (drop 1 is)
        p2 = numLt is (drop 3 is)

numLt :: [Int] -> [Int] -> Int
numLt xs ys = count id $ zipWith (<) xs ys
