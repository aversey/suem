{-# LANGUAGE BinaryLiterals #-}
-- This module organizes Emulator execution.
module Suem (Config(..), ConfigSocket(..), suem) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString as B
import Prelude hiding (Word)
import Data.Word
import Data.IORef
import Data.Foldable
import Control.Monad.Reader (runReaderT)
import Machine
import Commands
import Utils


-------------------------------------------------------------------------------
-- Main loop and command deciphering.

doCommand :: Word16 -> Emulator ()
doCommand 0b0000000000111100 = doORICCR
doCommand 0b0000000001111100 = doORISR
doCommand 0b0000001000111100 = doANDICCR
doCommand 0b0000001001111100 = doANDISR
doCommand 0b0000101000111100 = doEORICCR
doCommand 0b0000101001111100 = doEORISR
doCommand 0b0100101011111100 = doILLEGAL
doCommand 0b0100111001110000 = doRESET
doCommand 0b0100111001110001 = doNOP
doCommand 0b0100111001110010 = doSTOP
doCommand 0b0100111001110011 = doRTE
doCommand 0b0100111001110101 = doRTS
doCommand 0b0100111001110110 = doTRAPV
doCommand 0b0100111001110111 = doRTR
doCommand cmd
    | (extractBits cmd [0..7]) == 0b00000000 =
        doORI (extractBits cmd [8..9])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b00000010 =
        doANDI (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b00000100 =
        doSUBI (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b00000110 =
        doADDI (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b00001010 =
        doEORI (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b00001100 =
        doCMPI (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0000 &&
      (extractBits cmd [7]) == 0b0 &&
      (extractBits cmd [10..12]) == 0b001 =
        doMOVEP (extractBits cmd [4..6])
                (extractBits cmd [8])
                (extractBits cmd [9])
                (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0000 &&
      (extractBits cmd [8..9]) == 0b00 =
        doBTST (extractBits cmd [4..6])
               (extractBits cmd [7])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0000 &&
      (extractBits cmd [8..9]) == 0b01 =
        doBCHG (extractBits cmd [4..6])
               (extractBits cmd [7])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0000 &&
      (extractBits cmd [8..9]) == 0b10 =
        doBCLR (extractBits cmd [4..6])
               (extractBits cmd [7])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0000 &&
      (extractBits cmd [8..9]) == 0b11 =
        doBSET (extractBits cmd [4..6])
               (extractBits cmd [7])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..1]) == 0b00 &&
      (extractBits cmd [7..9]) == 0b001 =
        doMOVEA (extractBits cmd [2..3])
                (extractBits cmd [4..6])
                (extractBits cmd [10..12])
                (extractBits cmd [13..15])
    | (extractBits cmd [0..1]) == 0b00 =
        doMOVE (extractBits cmd [2..3])
               (extractBits cmd [4..6])
               (extractBits cmd [7..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100000011 =
        doSRMOVE (extractBits cmd [10..12])
                 (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100010011 =
        doMOVECCR (extractBits cmd [10..12])
                  (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100011011 =
        doMOVESR (extractBits cmd [10..12])
                 (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01000000 =
        doNEGX (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01000010 =
        doCLR (extractBits cmd [8..9])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01000100 =
        doNEG (extractBits cmd [8..9])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01000110 =
        doNOT (extractBits cmd [8..9])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..8]) == 0b010010001 &&
      (extractBits cmd [10..12]) == 0b000 =
        doEXT (extractBits cmd [9])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100100000 =
        doNBCD (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..12]) == 0b0100100001000 =
        doSWAP (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100100001 =
        doPEA (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100100011 =
        doTAS (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01001010 =
        doTST (extractBits cmd [8..9])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..11]) == 0b010011100100 =
        doTRAP (extractBits cmd [12..15])
    | (extractBits cmd [0..12]) == 0b0100111001010 =
        doLINK (extractBits cmd [13..15])
    | (extractBits cmd [0..12]) == 0b0100111001011 =
        doUNLK (extractBits cmd [13..15])
    | (extractBits cmd [0..11]) == 0b010011100110 =
        doMOVEUSP (extractBits cmd [12])
                  (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100111010 =
        doJSR (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..9]) == 0b0100111011 =
        doJMP (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..4]) == 0b01001 &&
      (extractBits cmd [6..8]) == 0b001 =
        doMOVEM (extractBits cmd [5])
                (extractBits cmd [9])
                (extractBits cmd [10..12])
                (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0100 &&
      (extractBits cmd [7..9]) == 0b111 =
        doLEA (extractBits cmd [4..6])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0100 &&
      (extractBits cmd [7..9]) == 0b110 =
        doCHK (extractBits cmd [4..6])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0101 &&
      (extractBits cmd [8..12]) == 0b11001 =
        doDBcc (extractBits cmd [4..7])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0101 &&
      (extractBits cmd [8..9]) == 0b11 =
        doScc (extractBits cmd [4..7])
              (extractBits cmd [10..12])
              (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0101 &&
      (extractBits cmd [7]) == 0b0 =
        doADDQ (extractBits cmd [4..6])
               (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b0101 &&
      (extractBits cmd [7]) == 0b1 =
        doSUBQ (extractBits cmd [4..6])
               (extractBits cmd [8..9])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..7]) == 0b01100000 =
        doBRA (extractBits cmd [8..15])
    | (extractBits cmd [0..7]) == 0b01100000 =
        doBSR (extractBits cmd [8..15])
    | (extractBits cmd [0..3]) == 0b0110 =
        doBcc (extractBits cmd [4..7])
              (extractBits cmd [8..15])
    | (extractBits cmd [0..3]) == 0b0111 &&
      (extractBits cmd [7]) == 0b0 =
        doMOVEQ (extractBits cmd [4..6])
                (extractBits cmd [8..15])
    | (extractBits cmd [0..3]) == 0b1000 &&
      (extractBits cmd [7..9]) == 0b011 =
        doDIVU (extractBits cmd [4..6])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b1000 &&
      (extractBits cmd [7..9]) == 0b111 =
        doDIVS (extractBits cmd [4..6])
               (extractBits cmd [10..12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b1000 &&
      (extractBits cmd [7..11]) == 0b10000 =
        doSBCD (extractBits cmd [4..6])
               (extractBits cmd [12])
               (extractBits cmd [13..15])
    | (extractBits cmd [0..3]) == 0b1000 =
        doOR (extractBits cmd [4..6])
             (extractBits cmd [7])
             (extractBits cmd [8..9])
             (extractBits cmd [10..12])
             (extractBits cmd [13..15])
-- doCommand [1,0,0,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doSUBA [u,v,w] [t] [a,b,c] [x,y,z]
-- doCommand [1,0,0,1,u,v,w,1, i,j,0,0,t,x,y,z] = doSUBX [u,v,w] [i,j] [t] [x,y,z]
-- doCommand [1,0,0,1,u,v,w,t, i,j,a,b,c,x,y,z] = doSUB [u,v,w] [t] [i,j] [a,b,c] [x,y,z]

-- doCommand [1,0,1,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doCMPA [u,v,w] [t] [a,b,c] [x,y,z]
-- doCommand [1,0,1,1,u,v,w,0, i,j,a,b,c,x,y,z] = doCMP [u,v,w] [i,j] [a,b,c] [x,y,z]
-- doCommand [1,0,1,1,u,v,w,1, i,j,0,0,1,x,y,z] = doCMPM [u,v,w] [i,j] [x,y,z]
-- doCommand [1,0,1,1,u,v,w,1, i,j,a,b,c,x,y,z] = doEOR [u,v,w] [i,j] [a,b,c] [x,y,z]

-- doCommand [1,1,0,0,u,v,w,0, 1,1,a,b,c,x,y,z] = doMULU [u,v,w] [a,b,c] [x,y,z]
-- doCommand [1,1,0,0,u,v,w,1, 1,1,a,b,c,x,y,z] = doMULS [u,v,w] [a,b,c] [x,y,z]
-- doCommand [1,1,0,0,u,v,w,1, 0,0,0,0,t,x,y,z] = doABCD [u,v,w] [t] [x,y,z]
-- doCommand [1,1,0,0,u,v,w,1, i,j,0,0,t,x,y,z] = doEXG [u,v,w] [i,j] [t] [x,y,z]
-- doCommand [1,1,0,0,u,v,w,t, i,j,a,b,c,x,y,z] = doAND [u,v,w] [t] [i,j] [a,b,c] [x,y,z]

-- doCommand [1,1,0,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doADDA [u,v,w] [t] [a,b,c] [x,y,z]
-- doCommand [1,1,0,1,u,v,w,1, i,j,0,0,t,x,y,z] = doADDX [u,v,w] [i,j] [t] [x,y,z]
-- doCommand [1,1,0,1,u,v,w,t, i,j,a,b,c,x,y,z] = doADD [u,v,w] [t] [i,j] [a,b,c] [x,y,z]

-- doCommand [1,1,1,0,0,0,0,t, 1,1,a,b,c,x,y,z] = doASD [t] [a,b,c] [x,y,z]
-- doCommand [1,1,1,0,0,0,1,t, 1,1,a,b,c,x,y,z] = doLSD [t] [a,b,c] [x,y,z]
-- doCommand [1,1,1,0,0,1,0,t, 1,1,a,b,c,x,y,z] = doROXd [t] [a,b,c] [x,y,z]
-- doCommand [1,1,1,0,0,1,1,t, 1,1,a,b,c,x,y,z] = doROd [t] [a,b,c] [x,y,z]
-- doCommand [1,1,1,0,u,v,w,t, i,j,a,0,0,x,y,z] = doADSR [u,v,w] [t] [i,j] [a] [x,y,z]
-- doCommand [1,1,1,0,u,v,w,t, i,j,a,0,1,x,y,z] = doLSDR [u,v,w] [t] [i,j] [a] [x,y,z]
-- doCommand [1,1,1,0,u,v,w,t, i,j,a,1,0,x,y,z] = doROXdR [u,v,w] [t] [i,j] [a] [x,y,z]
-- doCommand [1,1,1,0,u,v,w,t, i,j,a,1,1,x,y,z] = doROdR [u,v,w] [t] [i,j] [a] [x,y,z]
    | otherwise = error "Bad command."

runMachine :: Emulator ()
runMachine = forM_ [0..] $ \_ -> do
    pc <- with pc $ \pc -> readIORef pc
    cmd <- getWord $ fromIntegral pc
    doCommand cmd


-------------------------------------------------------------------------------
-- Config and start of execution based on the config.

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

makeMachine :: VM.IOVector Byte -> V.Vector Byte -> IO Machine
makeMachine ramData romData = do
    pc <- newIORef pcval
    sr <- newIORef 0x2700
    drs <- newIORef (fromIntegral 0, fromIntegral 0, fromIntegral 0,
                     fromIntegral 0, fromIntegral 0, fromIntegral 0,
                     fromIntegral 0, fromIntegral 0)
    ars <- newIORef (fromIntegral 0, fromIntegral 0, fromIntegral 0,
                     fromIntegral 0, fromIntegral 0, fromIntegral 0,
                     fromIntegral 0)
    usp <- newIORef 0
    ssp <- newIORef sspval
    return $ Machine pc sr drs ars usp ssp ramData romData
    where pcval = (fromIntegral $ romData V.! 4) * 256 * 256 * 256 +
                  (fromIntegral $ romData V.! 5) * 256 * 256 +
                  (fromIntegral $ romData V.! 6) * 256 +
                  (fromIntegral $ romData V.! 7)
          sspval = (fromIntegral $ romData V.! 0) * 256 * 256 * 256 +
                   (fromIntegral $ romData V.! 1) * 256 * 256 +
                   (fromIntegral $ romData V.! 2) * 256 +
                   (fromIntegral $ romData V.! 3)

runEmulator :: Emulator a -> Machine -> IO a
runEmulator (Emulator reader) m = runReaderT reader m

suem :: Config -> IO ()
suem (Config _ ramSize romPath _ _ _ _ _ _ _ _) = do
    romData <- B.readFile romPath
    ram <- VM.replicate ramSize 0
    m <- makeMachine ram (V.fromList $ B.unpack $ romData)
    runEmulator runMachine m
