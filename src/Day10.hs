module Day10 where

import Utils (showSolutions)
import Text.Megaparsec hiding (chunk)
import Parsing
import Prelude hiding (many)
import qualified Data.Text as T

solve :: Text -> Text
solve input = showSolutions p1 p2
  where theLines = T.lines input
        p1 = part1 theLines
        p2 = ()

part1 :: [Text] -> Int
part1 = sum . mapMaybe errorScore

errorScore :: Text -> Maybe Int
errorScore line = case parse chunk "a line" line of
  Left (ParseErrorBundle errs _state) -> case simplifyError $ head errs of
    UnexpectedChar c -> case c of -- incorrectly closed lines
      ')' -> Just 3
      ']' -> Just 57
      '}' -> Just 1197
      '>' -> Just 25137
      _ -> error $ "how did a " <> show c <> " get in here?"
    _ -> Nothing -- incomplete lines
  _ -> Nothing

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
