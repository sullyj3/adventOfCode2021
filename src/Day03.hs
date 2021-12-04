module Day03 where

import           Data.Text
import qualified Data.Text as T

import Numeric.LinearAlgebra.Data
import Data.Vector.Storable as Vec
import Data.Bits


bitsAsInts :: Int -> Vector I
bitsAsInts i = Vec.generate 5 go where
    go idx = if testBit i idx then 1 else 0

boolVecToInt :: Vector Bool -> IntInt
boolVecToInt = undefined

solve :: Text -> Text
solve input = undefined where
  Just is = intList input
  bitCounts :: Vector I
  Sum bitCounts = foldMap (Sum . bitsAsInts) is
  
  isMostCommonBit cnt = cnt > (length is `div` 2)
  