module Main where

import Day02

main :: IO ()
main = do
  putTextLn . solve =<< readFileText "inputs/day02.txt"

