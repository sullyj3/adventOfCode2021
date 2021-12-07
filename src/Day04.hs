{-# language ImportQualifiedPost #-}

module Day04 where
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (newline, space)
import Utils (showSolutions, elimination)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (many)
import Control.Monad (foldM)
import Data.List (partition)
import Motif (replace)
import Parsing

type Board a = [[a]]

parseInput :: Parser ([Int], [Board Int])
parseInput = do
  drawnNumbers <- commaSeparatedInts
  space
  boards <- parseBoard `sepEndBy1` space
  eof
  pure (drawnNumbers, boards)


parseBoard :: Parser (Board Int)
parseBoard = do
  row1 <- parseRow
  rows <- replicateM 4 (newline *> parseRow)
  pure (row1:rows)

parseRow :: Parser [Int]
parseRow = replicateM 5 (space *> decimal)

solve :: Text -> Text
solve input = showSolutions finalScore p2

  where boards :: [Board Int]
        Right (drawnNumbers, boards) = parse parseInput "day04.txt" input
        justifiedBoards = (map . map . map) Just boards

        finalScore :: Score
        Left finalScore = foldM crossOutOnAllBoards justifiedBoards drawnNumbers


        p2 = part2 drawnNumbers justifiedBoards

type Score = Int

crossOutOnAllBoards :: [Board (Maybe Int)] -> Int -> Either Score [Board (Maybe Int)]
crossOutOnAllBoards boards' drawn = traverse (crossOutMaybeWin drawn) boards'

crossOutMaybeWin :: Int -> Board (Maybe Int) -> Either Score (Board (Maybe Int))
crossOutMaybeWin lastCalled board
  | isWin replaced = Left finalScore
  | otherwise = Right replaced
  where replaced = crossOut lastCalled board
        finalScore = lastCalled * (sum . concatMap catMaybes $ replaced)

crossOut :: Int -> Board (Maybe Int) -> Board (Maybe Int)
crossOut lastCalled board = replaced
  where replaced = map (replace (Just lastCalled) Nothing) board

-- >>> isWin $ replicate 5 [Right 1, Left 2, Right 1, Right 1, Right 1]
isWin :: Board (Maybe Int) -> Bool
isWin rows = rowWin || colWin
  where rowWin = any (all isNothing) rows
        colWin = any (all isNothing) (transpose rows)

part2 :: [Int] -> [Board (Maybe Int)] -> Score
part2 drawnNumbers boards = let
  remainingNumbers :: [Int]
  lastBoard :: Board (Maybe Int)
  (Just lastBoard, remainingNumbers) = 
    runState (elimination eliminateWinners boards)
             drawnNumbers
  Left finalScore = foldM (flip crossOutMaybeWin) lastBoard remainingNumbers
  in finalScore

eliminateWinners :: [Board (Maybe Int)] -> State [Int] [Board (Maybe Int)]
eliminateWinners boards = do
  remainingToCall <- get
  case uncons remainingToCall of
    Just (n, rest) -> do
      put rest
      let crossedOut = map (crossOut n) boards
          (winners, notWinners) = partition isWin crossedOut
      if not (null notWinners) 
        then pure notWinners 
        else case nonEmpty winners of
               Nothing -> error "unreachable"
               Just winners' -> pure [last winners']
    Nothing -> 
      error "No numbers left to call, and there is more than one board remaining!"
