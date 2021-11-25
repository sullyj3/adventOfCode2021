module Main where

import Lib

main :: IO ()
main = traverse_ (print . depth) [
    Node 1 []
  , Node 1 [Node 2 []]
  , Node 1 [Node 2 [Node 3 []]]
  , Node 1 [Node 2 [Node 3 []], Node 4 []]
  ]
