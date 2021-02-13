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

getbyte :: Machine -> Int -> Word8
getbyte m a | a < 0x8 = B.index (rom m) a
            | a < 0x7e0000 = if B.length (ram m) >= a then B.index (ram m) a else 0
            | a < 0x800000 = B.index (rom m) (a - 0x7e0000)
            | otherwise = 0



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

printmachine :: Machine -> IO ()
printmachine _ = putStrLn "Machine created."

suem :: Config -> IO ()
suem (Config _ ramsize rompath _ _ _ _ _ _ _ _) = do
    romdata <- B.readFile rompath
    printmachine (Machine regs ramdata romdata)
    where ramdata = (B.replicate ramsize 0)
          regs    = (Registers 0 0 (replicate 8 0) (replicate 7 0) 0 0)
