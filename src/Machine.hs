module Machine where

import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits

data Registers = Registers {
    pc  :: Word32,
    sr  :: Word16,
    drs :: [Word32], -- d0 to d7
    ars :: [Word32], -- a0 to a6
    usp :: Word32,   -- this is a7 in user mode
    ssp :: Word32    -- this is a7 in supermode
}

data Machine = Machine {
    regs :: Registers,
    ram  :: V.Vector Word8,
    rom  :: V.Vector Word8
}

isSupervisor :: Machine -> Bool
isSupervisor m = testBit (sr $ regs m) 2

getByte :: Machine -> Int -> Word8
getByte m a | a < 0x8 = rom m V.! a
            | a < 0x7e0000 = if V.length (ram m) >= a then ram m V.! a
                                                      else 0xff
            | a < 0x800000 = rom m V.! (a - 0x7e0000)
            | otherwise = 0xff

getWord :: Machine -> Int -> Word16 -- TODO: only even addresses are allowed
getWord m a = (fromIntegral $ getByte m a) * 256 +
              (fromIntegral $ getByte m (a + 1))

getLong :: Machine -> Int -> Word32 -- TODO: only even addresses are allowed
getLong m a = (fromIntegral $ getWord m a) * 256 * 256 +
              (fromIntegral $ getWord m (a + 2))
