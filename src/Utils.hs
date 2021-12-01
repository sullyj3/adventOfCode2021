module Utils where

import qualified Data.Text as T

intList :: Text -> Maybe [Int]
intList = traverse (readMaybe . T.unpack) . T.lines

tShow :: Show a => a -> Text
tShow = T.pack . show
