{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedLists #-}

module Day03 where

import Data.Bits (Bits (complement, setBit, testBit, (.&.)))
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Motif (count)
import Numeric.LinearAlgebra.Data (I, Vector)
import qualified Relude.Unsafe as Unsafe
import Utils

-- least significant bit first
-- >>> bitsAsInts 0b101
-- [1,0,1,0,0]
bitsAsInts :: Int -> Word32 -> Vector I
bitsAsInts bitLength i =
  Vec.generate
    bitLength
    ( \idx ->
        if i `testBit` idx then 1 else 0
    )

-- count the number of bits in each place value of a list of binary numbers
bitCounts :: Int -> [Word32] -> Vector I
bitCounts bitLength = sum . map (bitsAsInts bitLength)

-- >>> boolVecToBitsLE [False, True, True]
-- 6
boolVecToBitsLE :: Vector Bool -> Word32
boolVecToBitsLE = Vec.ifoldl' step 0
  where
    step :: Word32 -> Int -> Bool -> Word32
    step acc idx b
      | b = acc `setBit` idx
      | otherwise = acc

calculateGamma :: Int -> [Word32] -> Word32
calculateGamma bitLength is =
  boolVecToBitsLE . Vec.map isMostCommonBit . bitCounts bitLength $ is
  where
    len = fromIntegral $ length is

    isMostCommonBit :: I -> Bool
    isMostCommonBit cnt = cnt > (len `div` 2)

exampleReport :: [Word32]
exampleReport =
  [ 0b00100,
    0b11110,
    0b10110,
    0b10111,
    0b10101,
    0b01111,
    0b00111,
    0b11100,
    0b10000,
    0b11001,
    0b00010,
    0b01010
  ]

data RatingType = Oxygen | CO2

-- >>> calculateRating 5 Oxygen exampleReport
-- 23
-- >>> calculateRating 5 CO2 exampleReport
-- 10

calculateRating :: Int -> RatingType -> [Word32] -> Word32
calculateRating bitLength ratingType is = final
  where
    Just final =
      evalState
        (elimination (elimByBitCriteria ratingType) is)
        (bitLength - 1)

elimByBitCriteria :: RatingType -> [Word32] -> State Int [Word32]
elimByBitCriteria ratingType candidates = do
  bitIndex <- get
  let ones = count (`testBit` bitIndex) candidates
      zeros = length candidates - ones
      theBit = case ratingType of
        Oxygen -> ones >= zeros
        CO2 -> ones < zeros
      remaining = filter ((== theBit) . (`testBit` bitIndex)) candidates
  put $ bitIndex - 1
  pure remaining

-- iteratively pare down a list of candidates, returning the final candidate,
-- if it exists
elimination :: Monad m => ([a] -> m [a]) -> [a] -> m (Maybe a)
elimination eliminateFrom = loop
  where
    loop = \case
      [] -> pure Nothing
      [x] -> pure (Just x)
      remaining -> do
        remaining' <- eliminateFrom remaining
        loop remaining'

solve :: Text -> Text
solve input = showSolutions p1 p2
  where
    Just is = binaryLines input

    bitLength :: Int
    bitLength = T.length . Unsafe.head . T.lines $ input

    gamma = calculateGamma bitLength is
    -- we mask to ensure irrelevant high bits are not set by complement
    nOnes n = 2 ^ n - 1
    mask = nOnes bitLength
    epsilon = complement gamma .&. mask
    p1 = gamma * epsilon

    oxygenRating = calculateRating bitLength Oxygen is
    co2Rating = calculateRating bitLength CO2 is
    p2 = oxygenRating * co2Rating
