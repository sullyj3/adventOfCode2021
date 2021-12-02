module Day02 where

import qualified Data.Text as T
import Linear.V2
import Linear.Vector
import Utils (showSolutions, tReadMaybe)

data Step = SUp Int | SDown Int | SForward Int
  deriving (Show)

-- >>> parse "forward 2\ndown 1"
-- Just [SForward 2,SDown 1]
parse :: Text -> Maybe [Step]
parse = traverse parseStep . T.lines

-- >>> parseStep "forward 5"
-- Just (SForward 5)
-- >>> parseStep "down 5"
-- Just (SDown 5)
parseStep :: Text -> Maybe Step
parseStep t = case T.words t of
  [instruction, tn] -> do
    n <- tReadMaybe tn
    case instruction of
      "forward" -> Just $ SForward n
      "up" -> Just $ SUp n
      "down" -> Just $ SDown n
      _ -> Nothing
  _ -> Nothing

p1StepToVec :: Step -> V2 Int
p1StepToVec = \case
  SForward n -> V2 n 0
  SDown n -> V2 0 n
  SUp n -> V2 0 (-1 * n)

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just steps = parse input
    -- part 1
    Sum (V2 a b) = foldMap (Sum . p1StepToVec) steps
    p1 = a * b

    -- part 2
    p2 = x * y
    x, y :: Int
    (V2 x y, _aim) = foldl' runStep (V2 0 0, 0) steps
      where
        runStep (pos, aim) = \case
          SUp n -> (pos, aim - n)
          SDown n -> (pos, aim + n)
          SForward n -> (pos ^+^ (V2 n (aim * n)), aim)