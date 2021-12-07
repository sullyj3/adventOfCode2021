module Parsing where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

commaSeparatedInts :: Parser [Int]
commaSeparatedInts = decimal `sepBy` single ','
