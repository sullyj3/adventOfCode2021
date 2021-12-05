module Day05 where

import Data.Map qualified as Map
import Linear.V2 (V2 (..))
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    parseMaybe,
    sepEndBy1,
    single,
  )
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (showSolutions)

data Vent = Vent (V2 Int) (V2 Int)
  deriving (Show)

type Parser = Parsec Void Text

v2 :: Parser (V2 Int)
v2 = V2 <$> (decimal <* single ',') <*> decimal

vent :: Parser Vent
vent = Vent <$> (v2 <* string " -> ") <*> v2

parseInput :: Parser [Vent]
parseInput = sepEndBy1 vent newline <* eof

solve :: Text -> Text
solve input = showSolutions (part1 vents) (part2 vents)
  where
    Just vents = parseMaybe parseInput input

part1 :: [Vent] -> Int
part1 vents = Map.size overlappingPoints
  where
    noDiags = filter isHorizontalOrVertical vents
    PC pointCounts = foldMap ventPointCounts noDiags
    overlappingPoints = Map.filter (>= 2) pointCounts

part2 :: [Vent] -> Int
part2 vents = Map.size overlappingPoints
  where
    PC pointCounts = foldMap ventPointCounts vents
    overlappingPoints = Map.filter (>= 2) pointCounts

isHorizontalOrVertical :: Vent -> Bool
isHorizontalOrVertical (Vent (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

newtype PointCounter = PC (Map (V2 Int) (Sum Int))
  deriving (Show)

instance Semigroup PointCounter where
  PC m1 <> PC m2 = PC $ Map.unionWith (<>) m1 m2

instance Monoid PointCounter where
  mempty = PC mempty

pcSingleton :: V2 Int -> PointCounter
pcSingleton v = PC $ Map.singleton v 1

-- >>> ventPoints (Vent (V2 1 0) (V2 1 5))
-- [V2 1 0,V2 1 1,V2 1 2,V2 1 3,V2 1 4,V2 1 5]
-- >>> ventPoints (Vent (V2 0 0) (V2 0 9))
-- [V2 0 0,V2 0 1,V2 0 2,V2 0 3,V2 0 4,V2 0 5,V2 0 6,V2 0 7,V2 0 8,V2 0 9]
-- >>> ventPoints (Vent (V2 0 0) (V2 1 1))
-- >>> ventPoints (Vent (V2 1 1) (V2 0 0))
-- >>> ventPoints (Vent (V2 2 0) (V2 0 2))
-- >>> ventPoints (Vent (V2 0 2) (V2 2 0))
-- [V2 0 0,V2 1 1]
-- [V2 0 0,V2 1 1]
-- [V2 0 2,V2 1 1,V2 2 0]
-- [V2 0 2,V2 1 1,V2 2 0]
ventPoints :: Vent -> [V2 Int]
ventPoints (Vent u v)
  | lx == rx =
    let [smallY, bigY] = sort [ly, ry]
     in [V2 lx y | y <- [smallY .. bigY]]
  | ly == ry = [V2 x ly | x <- [lx .. rx]]
  | ly <= ry = takeWhile (`notRightOf` rx) $ iterate (+ V2 1 1) left
  | ly > ry = takeWhile (`notRightOf` rx) $ iterate (+ V2 1 (-1)) left
  | otherwise = error "vent is not vertical, horizontal, or 45 degree diagonal"
  where
    [left@(V2 lx ly), V2 rx ry] = sort [u, v]

    notRightOf (V2 x _) r = x <= r

-- >>> ventPointCounts (Vent (V2 1 0) (V2 1 5))
-- >>> ventPointCounts (Vent (V2 0 0) (V2 0 9))
-- PC (fromList [(V2 1 0,Sum {getSum = 1}),(V2 1 1,Sum {getSum = 1}),(V2 1 2,Sum {getSum = 1}),(V2 1 3,Sum {getSum = 1}),(V2 1 4,Sum {getSum = 1}),(V2 1 5,Sum {getSum = 1})])
-- PC (fromList [(V2 0 0,Sum {getSum = 1}),(V2 0 1,Sum {getSum = 1}),(V2 0 2,Sum {getSum = 1}),(V2 0 3,Sum {getSum = 1}),(V2 0 4,Sum {getSum = 1}),(V2 0 5,Sum {getSum = 1}),(V2 0 6,Sum {getSum = 1}),(V2 0 7,Sum {getSum = 1}),(V2 0 8,Sum {getSum = 1}),(V2 0 9,Sum {getSum = 1})])
ventPointCounts :: Vent -> PointCounter
ventPointCounts = foldMap pcSingleton . ventPoints

-- >>> pcSingleton (V2 1 1) <> pcSingleton (V2 1 1)
-- PC (fromList [(V2 1 1,Sum {getSum = 2})])

testInput :: IO Text
testInput = do
  readFileText "inputs/day05example.txt"

testVents :: IO [Vent]
testVents = do
  fromMaybe (error "no parse") . parseMaybe parseInput <$> testInput
