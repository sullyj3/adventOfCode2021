{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedLists #-}

module Day03 where

import Data.Bits ( Bits((.&.), testBit, setBit, complement) )
import qualified Data.Text as T
import qualified Data.Vector.Storable as Vec
import Numeric.LinearAlgebra.Data (I, Vector)
import qualified Relude.Unsafe as Unsafe
import Utils (binaryLines, showSolutions)

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

testInputP1 :: [Word32]
testInputP1 =
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

-- >>> part1 5 testInputP1
-- 198
part1 :: Int -> [Word32] -> Word32
part1 bitLength is = gamma * epsilon
  where
    gamma = calculateGamma bitLength is
    -- we mask to ensure irrelevant high bits are not set by complement
    nOnes n = 2 ^ n - 1
    mask = nOnes bitLength
    epsilon = complement gamma .&. mask


solve :: Text -> Text
solve input = showSolutions (part1 bitLength is) ()
  where
    bitLength :: Int
    bitLength = T.length . Unsafe.head . T.lines $ input

    Just is = binaryLines input
