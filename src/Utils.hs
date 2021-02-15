module Utils where

import Data.Word
import Data.Bits
import Machine

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

toBits :: Word16 -> [Int]
toBits x = map (boolToInt . testBit x) [0..(finiteBitSize x-1)]

fromBits :: [Int] -> Int
fromBits = foldl (\a b -> 2 * a + b) 0 . reverse

args2 :: (Int -> Int -> Machine -> Machine) ->
         [Int] -> [Int] -> Machine -> Machine
args2 f a b = f (fromBits a) (fromBits b)

args3 :: (Int -> Int -> Int -> Machine -> Machine) ->
         [Int] -> [Int] -> [Int] -> Machine -> Machine
args3 f a b c = f (fromBits a) (fromBits b) (fromBits c)

args4 :: (Int -> Int -> Int -> Int -> Machine -> Machine) ->
         [Int] -> [Int] -> [Int] -> [Int] -> Machine -> Machine
args4 f a b c d = f (fromBits a) (fromBits b) (fromBits c) (fromBits d)

args5 :: (Int -> Int -> Int -> Int -> Int -> Machine -> Machine) ->
         [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Machine -> Machine
args5 f a b c d e = f (fromBits a) (fromBits b) (fromBits c)
                      (fromBits d) (fromBits e)
