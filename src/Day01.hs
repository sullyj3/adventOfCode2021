module Day01 where

import Utils (intList)
import Motif (count)

part1 :: FilePath -> IO Int
part1 fp = do
  Just is <- intList <$> readFileText fp
  pure $ numIncreases is

part2 :: FilePath -> IO Int
part2 fp = do
  Just is <- intList <$> readFileText fp
  pure $ numIncreases . map sum . sliding 3 $ is

numIncreases :: [Int] -> Int
numIncreases is = case nonEmpty is of
  Nothing -> 0
  Just (_ :| []) -> 0
  Just is' -> count id $ zipWith (<) (init is') (tail is')

sliding n [] = []
sliding n l@(_:rest) = case maybeTake n l of
  Just window -> window : sliding n rest
  Nothing -> [ ]

maybeTake :: Int -> [a] -> Maybe [a]
maybeTake 0 xs = Just []
maybeTake n [] = Nothing
maybeTake n (x:xs) = (x:) <$> maybeTake (n-1) xs

