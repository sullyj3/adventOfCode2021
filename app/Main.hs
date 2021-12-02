module Main where

import qualified Day01
import qualified Day02
import Data.Maybe (fromJust)

currentDay :: Int
currentDay = 2

day :: IO Int
day = getArgs <&> \case
  [] -> currentDay
  [ns] -> readMaybe ns ?: error "Expecting the argument to be a day number!"
  _ -> error "Expecting 0 or 1 arguments for the day to execute"

main :: IO ()
main = do
  (solve, input) <- day <&>
    \case
      1 -> (Day01.solve, "day01.txt")
      2 -> (Day02.solve, "day02.txt")
  putTextLn . solve =<< readFileText ("inputs/" ++ input)
