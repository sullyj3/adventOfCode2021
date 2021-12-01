module Main where

import Day01

main :: IO ()
main = do
  putTextLn . solve =<< readFileText "inputs/day01part1input.txt"

