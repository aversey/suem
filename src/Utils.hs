-- This module describes utility functions.
module Utils where

import Prelude hiding (Word)
import Data.Bits


-------------------------------------------------------------------------------
-- Bitwork

toBit :: Bool -> Int
toBit True  = 1
toBit False = 0

toBits :: Bits a => a -> [Int] -> [Int]
toBits x r = map (toBit . testBit x) r

toBitsWhole :: FiniteBits a => a -> [Int]
toBitsWhole x = toBits x [0..(finiteBitSize x - 1)]

fromBits :: [Int] -> Int
fromBits = foldl (\a b -> 2 * a + b) 0 . reverse

extractBits :: Bits a => a -> [Int] -> Int
extractBits x r = fromBits $ toBits x r


-------------------------------------------------------------------------------
-- Transformers for commands arguments

args2 :: (Int -> Int -> t) ->
         [Int] -> [Int] -> t
args2 f a b = f (fromBits a) (fromBits b)

args3 :: (Int -> Int -> Int -> t) ->
         [Int] -> [Int] -> [Int] -> t
args3 f a b c = f (fromBits a) (fromBits b) (fromBits c)

args4 :: (Int -> Int -> Int -> Int -> t) ->
         [Int] -> [Int] -> [Int] -> [Int] -> t
args4 f a b c d = f (fromBits a) (fromBits b) (fromBits c) (fromBits d)

args5 :: (Int -> Int -> Int -> Int -> Int -> t) ->
         [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> t
args5 f a b c d e = f (fromBits a) (fromBits b) (fromBits c)
                      (fromBits d) (fromBits e)
