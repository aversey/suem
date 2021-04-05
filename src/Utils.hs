-- This module describes utility functions.
module Utils where

import Data.Bits
import Data.Word (Word8, Word16, Word32)


-------------------------------------------------------------------------------
-- Bitwork

toBit :: Bool -> Int
toBit True  = 1
toBit False = 0

toBits :: FiniteBits a => a -> [Int]
toBits x = reverse $ map (toBit . testBit x) [0..finiteBitSize x - 1]

fromBits :: [Int] -> Int
fromBits = foldl (\a b -> 2 * a + b) 0

extractBits :: FiniteBits a => a -> [Int] -> Int
extractBits x r = fromBits $ map (\i -> toBits x !! i) r


-------------------------------------------------------------------------------
-- Size Convertion

convertLong :: Word32 -> Int -> Word32
convertLong x 1 = x .&. 0x000000FF
convertLong x 2 = x .&. 0x0000FFFF
convertLong x 4 = x
convertLong _ s = error $ "Wrong size (" ++ show s ++ ") of convertLong"

combineLong :: Word32 -> Word32 -> Int -> Word32
combineLong update base 1 = base .&. 0xFFFFFF00 .|. (convertLong update 1)
combineLong update base 2 = base .&. 0xFFFF0000 .|. (convertLong update 2)
combineLong update _    4 = update
combineLong _      _    s = error $
    "Wrong size (" ++ show s ++ ") of combineLong"

-------------------------------------------------------------------------------
-- Size Casting

getSize :: Int -> Int
getSize 0 = 1
getSize 1 = 2
getSize 2 = 4

getShortSize :: Int -> Int
getShortSize 0 = 2
getShortSize 1 = 4

getMoveSize :: Int -> Int
getMoveSize 1 = 1
getMoveSize 3 = 2
getMoveSize 2 = 4

-------------------------------------------------------------------------------
-- Flag checker

checkNegative :: Word32 -> Int -> Bool
checkNegative x 1 = x >= 0x80
checkNegative x 2 = x >= 0x8000
checkNegative x 4 = x >= 0x80000000

checkZero :: Word32 -> Bool
checkZero 0 = True
checkZero _ = False

-- TODO: carry & overflow checkers

-------------------------------------------------------------------------------
-- Sign extender

signExtend :: Word32 -> Int -> Word32
signExtend x 1
    | x < 0x80 = x
    | otherwise = x + 0xffffff00
signExtend x 2
    | x < 0x8000 = x
    | otherwise = x + 0xffff0000
signExtend x 4 = x
