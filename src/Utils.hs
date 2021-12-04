module Utils where

import qualified Data.Text as T
import Numeric
import Data.Char (digitToInt, intToDigit)

intList :: Text -> Maybe [Int]
intList = traverse (readMaybe . T.unpack) . T.lines

-- >>> parseBinary "1010" :: Maybe Word8
-- Just 10
-- >>> parseBinary "11111" :: Maybe Word8
-- Just 31
parseBinary :: (Eq a, Num a) => Text -> Maybe a
parseBinary t = 
  case readInt 2 (`elem` binDigits) digitToInt (T.unpack t) of
    [(n,_)] -> Just n
    _ -> Nothing 
  where binDigits :: [Char]
        binDigits = ['0', '1']

-- >>> binaryLines "101\n001"
-- Just [5,1]
binaryLines :: Text -> Maybe [Word32]
binaryLines = traverse parseBinary . T.lines

showBin :: (Integral a, Show a) => a -> Text
showBin n = T.pack $ showIntAtBase 2 intToDigit n ""

tShow :: Show a => a -> Text
tShow = T.pack . show

tReadMaybe :: Read a => Text -> Maybe a
tReadMaybe = readMaybe . T.unpack

showSolutions :: (Show a, Show b) => a -> b -> Text
showSolutions p1 p2 =
  T.unlines ["Part 1: " <> tShow p1, "Part 2: " <> tShow p2]
