module Day09 where

import Utils (showSolutions)

import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import qualified Data.Text as T
import Flow
import Data.Char (digitToInt)
import Linear
import qualified Data.Set as Set

type Grid = Vector (Vector Int)
type Point = V2 Int

parseInput :: Text -> Grid
parseInput = T.lines .>
  map T.unpack .> map (map digitToInt) .> map V.fromList .> V.fromList

solve :: Text -> Text
solve input = showSolutions p1 p2
  where 
    grid = parseInput input
    p1, p2 :: Int
    (p1, p2) = flip runReader grid do
      riskLevels :: [Int] <- traverse riskLevel =<< lowPoints

      basinSizes <- traverse (basinSize) =<< lowPoints
      let threeBiggest = take 3 . sortOn Down $ basinSizes

      pure (sum riskLevels, product threeBiggest)

testData :: IO Grid
testData = readFileText "inputs/day09example.txt" <&> parseInput

lowPoints :: MonadReader Grid m => m [Point]
lowPoints = filterM (isLowPoint) =<< allCoords

allCoords :: MonadReader Grid m => m [Point]
allCoords = ask <&> \g ->
  [V2 x y | x <- [0 .. width g - 1], y <- [0 .. height g - 1]]

surrounds :: MonadReader Grid m => Point -> m [Point]
surrounds (V2 x y) =
  filterM inbounds [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

inbounds :: MonadReader Grid m => Point -> m Bool
inbounds (V2 x y) =
  ask <&> \g -> (0 <= x && x < width g) && (0 <=y && y < height g)

height :: Grid -> Int
height g = V.length g

width :: Grid -> Int
width g = V.length $ g ! 0

isLowPoint :: MonadReader Grid m => Point -> m Bool
isLowPoint pt = do
  val <- ix pt
  neighbourVals <- traverse ix =<< surrounds pt
  pure $ all (> val) neighbourVals

riskLevel :: MonadReader Grid m => Point -> m Int
riskLevel pt = (1 +) <$> ix pt

ix :: MonadReader Grid m => Point -> m Int
ix (V2 x y) = ask <&> \g -> (g ! y) ! x

isLowerThan :: MonadReader Grid m => Point -> Point -> m Bool
isLowerThan a b = liftA2 (<) (ix a) (ix b)

visit :: (MonadReader Grid m, MonadState (Set Point) m)
      => Point -> m ()
visit pt = do
  modify (Set.insert pt)
  neighbours <- filterM reachable =<< surrounds pt
  for_ neighbours \neighbour ->
    unlessM (visited neighbour) $ visit neighbour
  where
    reachable neighbour = do
      currVal <- ix pt
      neighbourVal <- ix neighbour
      pure $ neighbourVal < 9 && neighbourVal > currVal

    visited :: MonadState (Set (Point)) m => Point -> m Bool
    visited neighbour = get <&> \visitedSet -> 
      neighbour `Set.member` visitedSet

basinSize :: MonadReader Grid m => Point -> m Int
basinSize lowPoint = do
  g <- ask
  let basin = execState (runReaderT (visit lowPoint) g) mempty
  pure $ Set.size basin
