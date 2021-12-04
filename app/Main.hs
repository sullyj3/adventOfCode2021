module Main where

import qualified Day01
import qualified Day02
import qualified Day03

currentDay :: Int
currentDay = 3

getDay :: IO Int
getDay =
  getArgs <&> \case
    [] -> currentDay
    [ns] -> readMaybe ns ?: error "Expecting the argument to be a day number!"
    _ -> error "Expecting 0 or 1 arguments for the day to execute"

main :: IO ()
main = do
  day <- getDay
  let (solve, input) = case day of
        1 -> (Day01.solve, "day01.txt")
        2 -> (Day02.solve, "day02.txt")
        3 -> (Day03.solve, "day03.txt")
  putTextLn $ "Day " <> show day <> ":"
  putTextLn . solve =<< readFileText ("inputs/" ++ input)

