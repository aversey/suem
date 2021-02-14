module Suem (Config(..), ConfigSocket(..), suem) where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Machine
import Commands


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

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

toBits :: Word16 -> [Int]
toBits x = map (boolToInt . testBit x) [0..(finiteBitSize x-1)]

fromBits :: [Int] -> Int
fromBits = foldl (\a b -> 2 * a + b) 0 . reverse

doCommand :: [Int] -> Machine -> Machine
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,0,0] = doReset
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,0,1] = doNothing
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,1,0] = doStop
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,1,1] = doRTE
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,0,1] = doRTS
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,1,0] = doTrapV
--doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,1,1] = doRTR
--doCommand [0,1,0,0,1,0,1,0, 1,1,1,1,1,1,0,0] = doIllegal
--doCommand [0,1,0,0,1,0,1,0, 1,1,a,b,c,d,e,f] =
--    doTAS (fromBits [a,b,c]) (fromBits [d,e,f])
--doCommand [0,1,0,0,1,0,1,0, a,b,c,d,e,f,g,h] =
--    doTST (fromBits [a,b]) (fromBits [c,d,e]) (fromBits [f,g,h])
--doCommand [0,1,0,0,1,0,1,0, 0,1,0,0,a,b,c,d] =
--    doTrap (fromBits [a,b,c,d])
--doCommand [0,1,0,0,1,0,1,0, 0,1,0,1,0,a,b,c] =
--    doLink (fromBits [a,b,c])
doCommand [0,1,0,0,1,0,1,0, 0,1,0,1,1,a,b,c] =
    doUnlink (fromBits [a,b,c])
doCommand _ = error "Bad command."

runMachine :: Machine -> IO ()
runMachine m = do
    runMachine $ doCommand (toBits $ getWord m $ fromIntegral $ pc $ regs m) m

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
