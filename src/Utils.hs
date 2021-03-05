-- This module describes utility functions.
module Utils where

import Data.Bits


-------------------------------------------------------------------------------
-- Bitwork

toBit :: Bool -> Int
toBit True  = 1
toBit False = 0

toBits :: Bits a => a -> [Int] -> [Int]
toBits x r = map (toBit . testBit x) r

fromBits :: [Int] -> Int
fromBits = foldl (\a b -> 2 * a + b) 0 . reverse

extractBits :: Bits a => a -> [Int] -> Int
extractBits x r = fromBits $ toBits x r
