module Day01 where

import qualified Data.Text as T
import Motif (count)
import Utils (intList, showSolutions)

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just is = intList input
    p1 = numLt is (drop 1 is)

    -- a + b + c < b + c + d <=> a < d
    -- so we don't need the sliding window
    p2 = numLt is (drop 3 is)

-- | Count the number of times an element of the first list is less than the
-- corresponding element of the second list.
-- Note that if the lists are different lengths, there is no correspondence
-- between elements in the excess region, so it doesn't contribute to the count.
numLt :: [Int] -> [Int] -> Int
numLt xs ys = count id $ zipWith (<) xs ys