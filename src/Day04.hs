module Day04 where
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space)
import Utils (showSolutions)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (many)

type Board = [[Int]]
type Parser = Parsec Void Text

parseInput :: Parser ([Int], [Board])
parseInput = do
  drawnNumbers <- decimal `sepBy` single ','
  _ <- newline
  boards <- parseBoard `sepBy` newline
  space 
  pure (drawnNumbers, boards)

parseBoard :: Parser Board
parseBoard = parseRow `sepBy` newline

parseRow :: Parser [Int]
parseRow = replicateM 5 (space *> decimal)

solve :: Text -> Text
solve input = showSolutions drawnNumbers boards
  where Right (drawnNumbers, boards) = parse parseInput "day04.txt" input
        p1 = ()
        p2 = ()
