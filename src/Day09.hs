module Day09 where

import Utils (showSolutions)

import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import qualified Data.Text as T
import Flow
import Data.Char (digitToInt)
import Linear

type Grid = Vector (Vector Int)

parseInput :: Text -> Grid
parseInput = T.lines .>
  map T.unpack .> map (map digitToInt) .> map V.fromList .> V.fromList

solve :: Text -> Text
solve input = showSolutions p1 p2
  where parsed = parseInput input
        p1 = part1 parsed
        p2 = part2 parsed

testData :: IO Grid
testData = readFileText "inputs/day09example.txt" <&> parseInput

part1 :: Grid -> Int
part1 g = sum $ riskLevel g <$> lowPoints
  where
    lowPoints = filter (isLowPoint g) (allCoords g)

allCoords :: Grid -> [V2 Int]
allCoords g = [V2 x y | x <- [0 .. width g - 1], y <- [0 .. height g - 1]]

surrounds :: Grid -> V2 Int -> [V2 Int]
surrounds g (V2 x y) = 
  filter (inbounds g) [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

inbounds :: Grid -> V2 Int -> Bool
inbounds g (V2 x y) = (0 <= x && x < width g) && (0 <=y && y < height g)

height :: Grid -> Int
height g = V.length g

width :: Grid -> Int
width g = V.length $ g ! 0

isLowPoint :: Grid -> V2 Int -> Bool
isLowPoint g (V2 x y) = 
  let val = g ! y ! x
   in all (> val) [ g ! y' ! x' | V2 x' y' <- surrounds g (V2 x y)]

riskLevel :: Grid -> V2 Int -> Int
riskLevel g pt = 1 + (g `ix` pt)

ix :: Grid -> V2 Int -> Int
ix g (V2 x y) = (g ! y) ! x

part2 :: Grid -> ()
part2 = const ()
