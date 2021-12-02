{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Day02 where

import Utils (showSolutions, tReadMaybe, V2(..))
import qualified Data.Text as T
import System.Posix.Internals (c_dup2)

-- >>> parse "forward 2\ndown 1"
-- Just [SForward 2,SDown 1]
parse :: Text -> Maybe [Step]
parse = traverse parseStep . T.lines

data Step = SUp Int | SDown Int | SForward Int
  deriving Show

p1StepToVec :: Step -> V2
p1StepToVec = \case
  SForward n -> V2 n 0
  SDown n -> V2 0 n
  SUp n -> V2 0 (-1 * n)

-- vector addition
instance Semigroup V2 where
  V2 a1 b1 <> V2 a2 b2 = V2 (a1+a2) (b1+b2)

instance Monoid V2 where
  mempty = V2 0 0

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

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just steps = parse input
        -- part 1
        V2 a b = foldMap p1StepToVec steps
        p1 = a*b
        -- part 2

        p2 = x*y
        (V2 x y, _aim) = foldl' runStep (V2 0 0,0) steps
        runStep (pos, aim) = \case
          SUp n -> (pos, aim-n)
          SDown n -> (pos, aim+n)
          SForward n -> (pos <> V2 n (aim * n), aim)
      
