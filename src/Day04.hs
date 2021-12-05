{-# language ImportQualifiedPost #-}

module Day04 where
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (newline, space)
import Utils (showSolutions, elimination)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (many)
import Control.Monad (foldM)

type Board a = [[a]]

type Parser = Parsec Void Text

parseInput :: Parser ([Int], [Board Int])
parseInput = do
  drawnNumbers <- decimal `sepBy` single ','
  space
  boards <- parseBoard `sepEndBy1` space
  eof
  pure (drawnNumbers, boards)

{-
debugging

printBoard :: (Show a) => [[a]] -> IO ()
printBoard b = traverse_ print b *> putStrLn ""

printBoards = traverse_ printBoard

exampleInput :: IO ([Int], [Board (Maybe Int)])
exampleInput = do
  t <- readFileText "inputs/day04example.txt"
  let Just (ns, boards) = parseMaybe parseInput t
  pure (ns, (map . map . map) Just boards)
-}

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

        finalScore :: Score
        Left finalScore = foldM crossOutOnAllBoards ((map . map . map) Just boards) drawnNumbers


        p2 = ()

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
  where replaced = map (map replaceCalled) board
        replaceCalled = \case
          Just n | n == lastCalled -> Nothing
          x -> x


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
  in undefined

eliminateWinners :: [Board (Maybe Int)] -> State [Int] [Board (Maybe Int)]
eliminateWinners boards = do
  remainingToCall <- get
  case uncons remainingToCall of
    Just (n, rest) -> undefined
    Nothing -> 
      error "No numbers left to call, and there is more than one board remaining!"

  -- foldr f
  undefined
