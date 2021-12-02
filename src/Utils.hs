module Utils where

import qualified Data.Text as T

intList :: Text -> Maybe [Int]
intList = traverse (readMaybe . T.unpack) . T.lines

tShow :: Show a => a -> Text
tShow = T.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . T.unpack

showSolutions :: (Show a, Show b) => a -> b -> Text
showSolutions p1 p2 =
  T.unlines ["Part 1: " <> tShow p1, "Part 2: " <> tShow p2]