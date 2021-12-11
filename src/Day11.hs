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
import Data.Massiv.Array (U, D, Ix2(..))
import Data.Massiv.Array.Mutable qualified as MA

type EnergyLevel = Int

type Grid r = Matrix r Int
type MGrid s r = MMatrix s r Int

-- todo maybe vector? decide after reading problem
parseInput :: Text -> Maybe (Grid U)
parseInput = T.lines .> map T.unpack .> map (map digitToInt) .> A.fromListsM Seq

solve :: Text -> Text
solve input = showSolutions octopi p2
  where Just octopi = parseInput input
        p1 = ()
        p2 = ()

adjacent :: Ix2 -> [Ix2]
adjacent pt@(Ix2 x y) =
  delete pt [(x+dx) :. (y+dy) | dx <- [-1..1], dy <- [-1..1]]

inBounds :: Ix2 -> Bool
inBounds (x :. y) = 0 <= x && x < width && 0 <= y && y < height
  where (width, height) = (10, 10)


increaseEnergy :: Grid U -> Grid D
increaseEnergy = A.map succ


--
-- findGTNines :: MGrid s r -> [Ix2]
-- findGTNines = gridIMapMaybe 
--   (\pt energy -> guard (energy > 9) >> pure pt)

{-
step :: Grid -> Grid
step = increaseEnergy .> flash
  where

flash grid = runST do
  mgrid <- thaw grid
  let loop = do
        let toFlash = findGTNines mv
        when (not . null $ toFlash) do
          for_ toFlash \flashIx2 -> do
            let adjacents = filter inBounds (adjacent flashIx2)
            for_ adjacents (modifyGrid mgrid succ)
          loop
  loop
  MV.unsafeFreeze mgrid

modifyGrid mgrid f (V2 x y) =
  MV.modify mgrid (\row -> Mv.modify row f x) y


gridIMapMaybe :: (Ix2 -> a -> Maybe b) -> Grid -> [b]
gridIMapMaybe f = concat . V.toList . V.imap 
  (\y row -> V.imapMaybe (\x -> f (V2 x y)))

part1 :: () -> ()
part1 = undefined

part2 :: () -> ()
part2 = undefined
-}
