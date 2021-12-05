module Day02 where

import Linear.V2 ( V2(..) )
import Utils (showSolutions)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Step = SUp Int | SDown Int | SForward Int
  deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser [Step]
parseInput = step `sepEndBy` newline

step :: Parser Step
step = do
  steptype <- try (SForward <$ string "forward") <|>
              try (SUp      <$ string "up)" <|>
                   SDown    <$ string "down" ) 
  space
  steptype <$> decimal

p1StepToVec :: Step -> V2 Int
p1StepToVec = \case
  SForward n -> V2 n 0
  SDown n -> V2 0 n
  SUp n -> V2 0 (-1 * n)

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just steps = parseMaybe parseInput input
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
          SForward n -> (pos + V2 n (aim * n), aim)
