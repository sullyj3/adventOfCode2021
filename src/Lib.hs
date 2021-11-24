
module Lib
    ( Tree(..)
    , depth
    ) where

import Polysemy
import Polysemy.NonDet
import Polysemy.State

import Flow
import Prelude hiding (State, evalState, get, modify)
import Relude.Unsafe


data Tree = Node Int [Tree]



-- >>> depth (Node 1 [])
-- 0
-- >>> depth (Node 1 [Node 2 []])
-- 1
-- >>> depth (Node 1 [Node 2 [Node 3 []]])
-- 2
-- >>> depth (Node 1 [Node 2 [Node 3 []], Node 4 []])
-- 2

depth :: Tree -> Int
depth t = depthSem t
  |> runNonDet
  |> evalState 0
  |> run
  |> maximum
  where maximum :: [Int] -> Int
        maximum = foldl' max minBound


depthSem :: Tree -> Sem [NonDet, State Int] Int
depthSem (Node _ []) = get
depthSem (Node _ children) = do
  modify (+1)
  t <- asumMap pure children
  depthSem t
