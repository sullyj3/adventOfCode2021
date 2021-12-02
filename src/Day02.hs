{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Day02 where

import Utils (showSolutions, tReadMaybe)
import qualified Data.Text as T

-- >>> parse "forward 2\ndown 1"
-- Just [(2,0),(0,1)]
parse :: Text -> Maybe [Step]
parse = traverse parseStep . T.lines

-- we represent each step by its vector offset
newtype Step = Step (Int, Int)
  deriving newtype Show

instance Semigroup Step where
  Step (a1, b1) <> Step (a2, b2) = Step (a1+a2, b1+b2)

instance Monoid Step where
  mempty = Step (0,0)

-- >>> parseStep "forward 5"
-- Just (5,0)
-- >>> parseStep "down 5"
-- Just (0,5)
parseStep :: Text -> Maybe Step
parseStep t = Step <$> case T.words t of
  [instruction, tn] -> do
    n <- tReadMaybe tn
    case instruction of
      "forward" -> Just (n, 0)
      "backward" -> Just (-1 * n, 0)
      "up" -> Just (0, -1 * n)
      "down" -> Just (0, n)
      _ -> Nothing
  _ -> Nothing

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just steps = parse input
        Step (a,b) = fold steps
        p1 = a*b
        p2 = ()
