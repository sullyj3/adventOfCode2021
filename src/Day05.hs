module Day05 where

import Utils (showSolutions)

import Text.Megaparsec
import Linear.V2
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (string, space, newline)

data Vent = Vent (V2 Int) (V2 Int)
  deriving Show

type Parser = Parsec Void Text

v2 :: Parser (V2 Int)
v2 = V2 <$> (decimal <* single ',') <*> decimal

vent :: Parser Vent
vent = Vent <$> (v2 <* string " -> ") <*> v2

parseInput :: Parser [Vent]
parseInput = sepEndBy1 vent newline <* eof

solve :: Text -> Text
solve input = showSolutions p1 p2
  where Just vents = parseMaybe parseInput input
        p1 = ()
        p2 = ()

testInput :: IO Text
testInput = do
  readFileText "inputs/day05example.txt"
