module Day10 where

import Utils (showSolutions)
import Text.Megaparsec hiding (chunk)
import Parsing
import Prelude hiding (many)
import Relude.Unsafe ((!!))
import qualified Data.Text as T
import Flow

solve :: Text -> Text
solve input = showSolutions p1 p2
  where theLines = T.lines input
        (errorScores, completionScores) = classifyLines theLines
        p1 = sum errorScores
        p2 = sort completionScores !! (length completionScores `div` 2)

type ErrorScore = Int
type CompletionScore = Int

chunkType :: Char -> Char -> Parser ()
chunkType open close = () <$
  between (single open) (try $ single close) (try $ many chunk)

chunk :: Parser ()
chunk = () <$ choice [
  (chunkType '(' ')'),
  (chunkType '[' ']'),
  (chunkType '{' '}'),
  (chunkType '<' '>')]


data SimpleError = UnexpectedChar Char | UnexpectedEOF

simplifyError :: ParseError Text e -> SimpleError
simplifyError = \case
  (TrivialError _ (Just (Tokens (char :| _))) _) -> UnexpectedChar char
  (TrivialError _ (Just EndOfInput) _) -> UnexpectedEOF
  _ -> error "unexpected error lmao what"

classifyLines :: [Text] -> ([ErrorScore], [CompletionScore])
classifyLines = partitionWith \line ->
  case parse chunk "a line" line of
    Left (ParseErrorBundle errs _state) -> case simplifyError $ head errs of
      -- incorrectly closed lines
      UnexpectedChar c -> Left $ errorScore c
      -- incomplete lines
      UnexpectedEOF -> Right (line |> completeLine |> scoreCompletion)
    e ->  error $ "unexpected error: " <> show e

-- Part 1
errorScore :: Char -> Int
errorScore = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  c -> error $ "how did a " <> show c <> " get in here?"

-- Part 2

completeLine :: Text -> Text
completeLine = T.pack . T.foldl' step ""
  where 
    -- we assume the string is incomplete, not closed incorrectly. 
    step acc = \case
      '(' -> ')' : acc
      ')' -> drop 1 acc
      '[' -> ']' : acc
      ']' -> drop 1 acc
      '{' -> '}' : acc
      '}' -> drop 1 acc
      '<' -> '>' : acc
      '>' -> drop 1 acc
      c -> error $ "unexpected character: " <> show c

scoreCompletion :: Text -> Int
scoreCompletion = T.foldl' step 0
  where 
    step :: Int -> Char -> Int
    step acc c = acc * 5 + case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> error $ "unexpected character: " <> show c
      

