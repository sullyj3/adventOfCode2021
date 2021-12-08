module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import Data.List ((!!), last)

data ToRun = Days [Int] | AllDays | Latest

parseArgs :: IO ToRun
parseArgs =
  getArgs <&> \case
    [] -> Latest
    ["all"] -> AllDays
    ns -> Days $ 
      traverse readMaybe ns ?: error "Expecting the argument to be a day number, or 'all'!"

main :: IO ()
main = do
  toRun <- parseArgs
  let days = [ (Day01.solve, "day01.txt")
             , (Day02.solve, "day02.txt")
             , (Day03.solve, "day03.txt")
             , (Day04.solve, "day04.txt")
             , (Day05.solve, "day05.txt")
             , (Day06.solve, "day06.txt")
             , (Day07.solve, "day07.txt")
             , (Day08.solve, "day08.txt") ]

      daysToRun :: [(Int, (Text -> Text, FilePath))]
      daysToRun = case toRun of
        AllDays -> [1..] `zip` days
        Days ns -> map (\dayNum -> (dayNum, days !! (dayNum - 1))) ns
        Latest -> [Data.List.last $ [1..] `zip` days]

  for_ daysToRun $ \(day, (solve, inputFP)) -> do
    putTextLn $ "Day " <> show day <> ":"
    putTextLn . solve =<< readFileText ("inputs/" ++ inputFP)
