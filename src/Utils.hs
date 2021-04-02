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
