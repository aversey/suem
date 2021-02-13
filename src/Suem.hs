module Suem (Config(..), ConfigSocket(..), suem) where

import qualified Data.ByteString as B
import Data.Word


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
    ram  :: B.ByteString,
    rom  :: B.ByteString
}

getByte :: Machine -> Int -> Word8
getByte m a | a < 0x8 = B.index (rom m) a
            | a < 0x7e0000 = if B.length (ram m) >= a then B.index (ram m) a
                                                      else 0xff
            | a < 0x800000 = B.index (rom m) (a - 0x7e0000)
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

runMachine :: Machine -> IO ()
runMachine _ = do
    putStrLn "Machine created."

makeMachine :: B.ByteString -> Int -> Machine
makeMachine romData ramSize = Machine regs ramData romData
    where ramData = (B.replicate ramSize 0)
          regs    = (Registers (getLong m 0x7e0004) 0x2700 (replicate 8 0)
                               (replicate 7 0) 0 (getLong m 0x7e0000))
          m       = Machine (Registers 0 0 [] [] 0 0) B.empty romData

suem :: Config -> IO ()
suem (Config _ ramSize romPath _ _ _ _ _ _ _ _) = do
    romData <- B.readFile romPath
    runMachine (makeMachine romData ramSize)
