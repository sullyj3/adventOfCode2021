module Day01 where

import Motif (count)
import Utils (intList, showSolutions)
import qualified Data.Text as T

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just is = intList input
        p1 = numIncreases is
        p2 = numIncreases . map sum . sliding 3 $ is

numIncreases :: [Int] -> Int
numIncreases is = case nonEmpty is of
  Nothing -> 0
  Just is' -> count id $ zipWith (<) (init is') (tail is')

sliding n [] = []
sliding n l@(_ : rest) = case maybeTake n l of
  Just window -> window : sliding n rest
  Nothing -> []

maybeTake :: Int -> [a] -> Maybe [a]
maybeTake 0 xs = Just []
maybeTake n [] = Nothing
maybeTake n (x : xs) = (x :) <$> maybeTake (n - 1) xs
