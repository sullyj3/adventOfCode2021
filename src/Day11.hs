module Day11 where

import Utils (showSolutions)
import Data.Text qualified as T
import Data.Char (digitToInt)
import Flow
-- import Data.Vector qualified as V
-- import Data.Vector (Vector)
-- import Data.Vector.Mutable qualified as MV
-- import Data.Vector.Mutable (MVector)
import Linear.V2
import Data.List (delete)
import Control.Monad.ST
import Data.Massiv.Core
import Data.Massiv.Array qualified as A
import Data.Massiv.Array (U)

type EnergyLevel = Int

type Grid = Matrix U Int
type MGrid s = MMatrix s U Int
type Point = V2 Int

-- todo maybe vector? decide after reading problem
parseInput :: Text -> Maybe Grid
parseInput = T.lines .> map T.unpack .> map (map digitToInt) .> A.fromListsM Seq

solve :: Text -> Text
solve input = showSolutions octopi p2
  where Just octopi = parseInput input
        p1 = ()
        p2 = ()

{-
adjacent :: Point -> [Point]
adjacent pt@(V2 x y) =
  delete pt [V2 (x+dx) (y+dy) | dx <- [-1..1], dy <- [-1..1]]

inBounds :: Point -> Bool
inBounds (V2 x y) = 0 <= x && x < width && 0 <= y && y < height
  where (width, height) = (10, 10)

step :: Grid -> Grid
step = increaseEnergy .> flash
  where
    increaseEnergy = V.map (V.map succ) g

flash grid = runST do
  mgrid <- thaw grid
  let loop = do
        let toFlash = findGTNines mv
        when (not . null $ toFlash) do
          for_ toFlash \flashPoint -> do
            let adjacents = filter inBounds (adjacent flashPoint)
            for_ adjacents (modifyGrid mgrid succ)
          loop
  loop
  MV.unsafeFreeze mgrid

modifyGrid mgrid f (V2 x y) =
  MV.modify mgrid (\row -> Mv.modify row f x) y

findGTNines :: MGrid s -> [Point]
findGTNines = gridIMapMaybe 
  (\pt energy -> guard (energy > 9) >> pure pt)

gridIMapMaybe :: (Point -> a -> Maybe b) -> Grid -> [b]
gridIMapMaybe f = concat . V.toList . V.imap 
  (\y row -> V.imapMaybe (\x -> f (V2 x y)))

part1 :: () -> ()
part1 = undefined

part2 :: () -> ()
part2 = undefined
-}
