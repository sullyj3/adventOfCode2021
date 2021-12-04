{-# language ImportQualifiedPost #-}

module Day04 where
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space)
import Utils (showSolutions)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (many)
import Data.Massiv.Array (Array, B, Ix2)
import Data.Massiv.Array qualified as MSV

type Board = Array B Ix2 Int

type Parser = Parsec Void Text

parseInput :: Parser ([Int], [Board])
parseInput = do
  drawnNumbers <- decimal `sepBy` single ','
  space
  boards <- parseBoard `sepEndBy1` space
  eof
  pure (drawnNumbers, boards)

parseBoard :: Parser Board
parseBoard = do
  row1 <- parseRow
  rows <- replicateM 4 (newline *> parseRow)
  pure $ MSV.fromLists' MSV.Seq (row1:rows) 

parseRow :: Parser [Int]
parseRow = replicateM 5 (space *> decimal)

solve :: Text -> Text
solve input = showSolutions drawnNumbers boards
  where Right (drawnNumbers, boards) = parse parseInput "day04.txt" input
        p1 = ()
        p2 = ()

oneBoard = unlines 
  [ "68 22 95 71 42"
  , "18  0 61 86 44"
  , "46 29 51 23 99"
  , "11 60 66 12 52"
  , "53 87 41 90 49"
  ]

