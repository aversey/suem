module Suem (Config(..), ConfigSocket(..), suem) where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B
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


data ConfigSocket = ConfigInet String | ConfigUnix String

data Config = Config Int      -- frequence
                     Int      -- size of RAM
                     FilePath -- path to ROM
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)
                     (Maybe ConfigSocket)

doCommand :: Word16 -> Machine -> Machine
doCommand cmd m = case cmd .&. 0xf000 of
    0 -> if testBit cmd 7
        then let rega = (shiftR cmd 9) .&. 0x7 in
            m
        else m
    0x1000 -> m
    0x2000 -> m
    0x3000 -> m
    0x4000 -> m
    0x5000 -> m
    0x6000 -> m
    0x7000 -> m
    0x8000 -> m
    0x9000 -> m
    0xb000 -> m
    0xc000 -> m
    0xd000 -> m
    0xe000 -> m
    _ -> error "Bad command"


runMachine :: Machine -> IO ()
runMachine m = do
    runMachine $ doCommand (getWord m $ fromIntegral $ pc $ regs m) m

makeMachine :: V.Vector Word8 -> Int -> Machine
makeMachine romData ramSize = Machine rs rd romData
    where rd = V.replicate ramSize 0
          rs = Registers (getLong m 0x7e0004) 0x2700 (replicate 8 0)
                         (replicate 7 0) 0 (getLong m 0x7e0000)
          m  = Machine (Registers 0 0 [] [] 0 0) V.empty romData

suem :: Config -> IO ()
suem (Config _ ramSize romPath _ _ _ _ _ _ _ _) = do
    romData <- B.readFile romPath
    runMachine (makeMachine (V.fromList $ B.unpack $ romData) ramSize)
