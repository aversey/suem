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
import Data.IP
import Network.Socket
import Numeric
import Machine
import Control
import Instructions
import Utils
import Device


------------------------------------------------------------------------------
-- Main loop and command deciphering.

doInstruction :: Word16 -> Emulator ()
doInstruction 0b0000000000111100 = doORICCR
doInstruction 0b0000000001111100 = doORISR
doInstruction 0b0000001000111100 = doANDICCR
doInstruction 0b0000001001111100 = doANDISR
doInstruction 0b0000101000111100 = doEORICCR
doInstruction 0b0000101001111100 = doEORISR
doInstruction 0b0100101011111100 = doILLEGAL
doInstruction 0b0100111001110000 = doRESET
doInstruction 0b0100111001110001 = doNOP
doInstruction 0b0100111001110010 = doSTOP
doInstruction 0b0100111001110011 = doRTE
doInstruction 0b0100111001110101 = doRTS
doInstruction 0b0100111001110110 = doTRAPV
doInstruction 0b0100111001110111 = doRTR
doInstruction opcode
    | (extractBits opcode [0..7]) == 0b00000000 =
        doORI (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b00000010 =
        doANDI (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b00000100 =
        doSUBI (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b00000110 =
        doADDI (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b00001010 =
        doEORI (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b00001100 =
        doCMPI (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0000 &&
      (extractBits opcode [7]) == 0b0 &&
      (extractBits opcode [10..12]) == 0b001 =
        doMOVEP (extractBits opcode [4..6])
                (extractBits opcode [8])
                (extractBits opcode [9])
                (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0000 &&
      (extractBits opcode [8..9]) == 0b00 =
        doBTST (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0000 &&
      (extractBits opcode [8..9]) == 0b01 =
        doBCHG (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0000 &&
      (extractBits opcode [8..9]) == 0b10 =
        doBCLR (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0000 &&
      (extractBits opcode [8..9]) == 0b11 =
        doBSET (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..1]) == 0b00 &&
      (extractBits opcode [7..9]) == 0b001 =
        doMOVEA (extractBits opcode [2..3])
                (extractBits opcode [4..6])
                (extractBits opcode [10..12])
                (extractBits opcode [13..15])
    | (extractBits opcode [0..1]) == 0b00 =
        doMOVE (extractBits opcode [2..3])
               (extractBits opcode [4..6])
               (extractBits opcode [7..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100000011 =
        doSRMOVE (extractBits opcode [10..12])
                 (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100010011 =
        doMOVECCR (extractBits opcode [10..12])
                  (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100011011 =
        doMOVESR (extractBits opcode [10..12])
                 (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01000000 =
        doNEGX (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01000010 =
        doCLR (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01000100 =
        doNEG (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01000110 =
        doNOT (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..8]) == 0b010010001 &&
      (extractBits opcode [10..12]) == 0b000 =
        doEXT (extractBits opcode [9])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100100000 =
        doNBCD (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..12]) == 0b0100100001000 =
        doSWAP (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100100001 =
        doPEA (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100100011 =
        doTAS (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01001010 =
        doTST (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..11]) == 0b010011100100 =
        doTRAP (extractBits opcode [12..15])
    | (extractBits opcode [0..12]) == 0b0100111001010 =
        doLINK (extractBits opcode [13..15])
    | (extractBits opcode [0..12]) == 0b0100111001011 =
        doUNLK (extractBits opcode [13..15])
    | (extractBits opcode [0..11]) == 0b010011100110 =
        doMOVEUSP (extractBits opcode [12])
                  (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100111010 =
        doJSR (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..9]) == 0b0100111011 =
        doJMP (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..4]) == 0b01001 &&
      (extractBits opcode [6..8]) == 0b001 =
        doMOVEM (extractBits opcode [5])
                (extractBits opcode [9])
                (extractBits opcode [10..12])
                (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0100 &&
      (extractBits opcode [7..9]) == 0b111 =
        doLEA (extractBits opcode [4..6])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0100 &&
      (extractBits opcode [7..9]) == 0b110 =
        doCHK (extractBits opcode [4..6])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0101 &&
      (extractBits opcode [8..12]) == 0b11001 =
        doDBcc (extractBits opcode [4..7])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0101 &&
      (extractBits opcode [8..9]) == 0b11 =
        doScc (extractBits opcode [4..7])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0101 &&
      (extractBits opcode [7]) == 0b0 =
        doADDQ (extractBits opcode [4..6])
               (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b0101 &&
      (extractBits opcode [7]) == 0b1 =
        doSUBQ (extractBits opcode [4..6])
               (extractBits opcode [8..9])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..7]) == 0b01100001 =
        doBSR (extractBits opcode [8..15])
    | (extractBits opcode [0..3]) == 0b0110 =
        doBcc (extractBits opcode [4..7])
              (extractBits opcode [8..15])
    | (extractBits opcode [0..3]) == 0b0111 &&
      (extractBits opcode [7]) == 0b0 =
        doMOVEQ (extractBits opcode [4..6])
                (extractBits opcode [8..15])
    | (extractBits opcode [0..3]) == 0b1000 &&
      (extractBits opcode [7..9]) == 0b011 =
        doDIVU (extractBits opcode [4..6])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1000 &&
      (extractBits opcode [7..9]) == 0b111 =
        doDIVS (extractBits opcode [4..6])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1000 &&
      (extractBits opcode [7..11]) == 0b10000 =
        doSBCD (extractBits opcode [4..6])
               (extractBits opcode [12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1000 =
        doOR (extractBits opcode [4..6])
             (extractBits opcode [7])
             (extractBits opcode [8..9])
             (extractBits opcode [10..12])
             (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1001 &&
      (extractBits opcode [8..9]) == 0b11 =
        doSUBA (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1001 &&
      (extractBits opcode [7]) == 0b1 &&
      (extractBits opcode [10..11]) == 0b00 =
        doSUBX (extractBits opcode [4..6])
               (extractBits opcode [8..9])
               (extractBits opcode [12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1001 =
        doSUB (extractBits opcode [4..6])
              (extractBits opcode [7])
              (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1011 &&
      (extractBits opcode [8..9]) == 0b11 =
        doCMPA (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1011 &&
      (extractBits opcode [7]) == 0b0 =
        doCMP (extractBits opcode [4..6])
              (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1011 &&
      (extractBits opcode [7]) == 0b1 &&
      (extractBits opcode [10..12]) == 0b001 =
        doCMPM (extractBits opcode [4..6])
               (extractBits opcode [8..9])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1011 &&
      (extractBits opcode [7]) == 0b1 =
        doEOR (extractBits opcode [4..6])
              (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1100 &&
      (extractBits opcode [7..9]) == 0b011 =
        doMULU (extractBits opcode [4..6])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1100 &&
      (extractBits opcode [7..9]) == 0b111 =
        doMULS (extractBits opcode [4..6])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1100 &&
      (extractBits opcode [7..11]) == 0b10000 =
        doABCD (extractBits opcode [4..6])
               (extractBits opcode [12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1100 &&
      (extractBits opcode [7]) == 0b1 &&
      (extractBits opcode [10..11]) == 0b00 =
        doEXG (extractBits opcode [4..6])
              (extractBits opcode [8..9])
              (extractBits opcode [12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1100 =
        doAND (extractBits opcode [4..6])
              (extractBits opcode [7])
              (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1101 &&
      (extractBits opcode [8..9]) == 0b11 =
        doADDA (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1101 &&
      (extractBits opcode [7]) == 0b1 &&
      (extractBits opcode [10..11]) == 0b00 =
        doADDX (extractBits opcode [4..6])
               (extractBits opcode [8..9])
               (extractBits opcode [12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1101 =
        doADD (extractBits opcode [4..6])
              (extractBits opcode [7])
              (extractBits opcode [8..9])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..6]) == 0b1110000 &&
      (extractBits opcode [8..9]) == 0b11 =
        doASD (extractBits opcode [7])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..6]) == 0b1110001 &&
      (extractBits opcode [8..9]) == 0b11 =
        doLSD (extractBits opcode [7])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..6]) == 0b1110010 &&
      (extractBits opcode [8..9]) == 0b11 =
        doROXd (extractBits opcode [7])
               (extractBits opcode [10..12])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..6]) == 0b1110011 &&
      (extractBits opcode [8..9]) == 0b11 =
        doROd (extractBits opcode [7])
              (extractBits opcode [10..12])
              (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1110 &&
      (extractBits opcode [11..12]) == 0b00 =
        doADSR (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [8..9])
               (extractBits opcode [10])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1110 &&
      (extractBits opcode [11..12]) == 0b01 =
        doLSDR (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [8..9])
               (extractBits opcode [10])
               (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1110 &&
      (extractBits opcode [11..12]) == 0b10 =
        doROXdR (extractBits opcode [4..6])
                (extractBits opcode [7])
                (extractBits opcode [8..9])
                (extractBits opcode [10])
                (extractBits opcode [13..15])
    | (extractBits opcode [0..3]) == 0b1110 &&
      (extractBits opcode [11..12]) == 0b11 =
        doROdR (extractBits opcode [4..6])
               (extractBits opcode [7])
               (extractBits opcode [8..9])
               (extractBits opcode [10])
               (extractBits opcode [13..15])
    | otherwise = do
                pc <- readPC
                sr <- readSR
                d0 <- readD 0 4
                d1 <- readD 1 4
                d2 <- readD 2 4
                d3 <- readD 3 4
                d4 <- readD 4 4
                d5 <- readD 5 4
                d6 <- readD 6 4
                d7 <- readD 7 4
                a0 <- readA 0 4
                a1 <- readA 1 4
                a2 <- readA 2 4
                a3 <- readA 3 4
                a4 <- readA 4 4
                a5 <- readA 5 4
                a6 <- readA 6 4
                a7 <- readA 7 4
                error ("Error:\n"
                    ++ "PC:0x" ++ showHex pc "\n"
                    ++ "SR:0x" ++ showHex sr "\n\n"
                    ++ "D0:0x" ++ showHex d0 "\n"
                    ++ "D1:0x" ++ showHex d1 "\n"
                    ++ "D2:0x" ++ showHex d2 "\n"
                    ++ "D3:0x" ++ showHex d3 "\n"
                    ++ "D4:0x" ++ showHex d4 "\n"
                    ++ "D5:0x" ++ showHex d5 "\n"
                    ++ "D6:0x" ++ showHex d6 "\n"
                    ++ "D7:0x" ++ showHex d7 "\n\n"
                    ++ "A0:0x" ++ showHex a0 "\n"
                    ++ "A1:0x" ++ showHex a1 "\n"
                    ++ "A2:0x" ++ showHex a2 "\n"
                    ++ "A3:0x" ++ showHex a3 "\n"
                    ++ "A4:0x" ++ showHex a4 "\n"
                    ++ "A5:0x" ++ showHex a5 "\n"
                    ++ "A6:0x" ++ showHex a6 "\n"
                    ++ "A7:0x" ++ showHex a7 "")

runMachine :: Emulator ()
runMachine = forM_ [0..] $ \_ -> do
    pc <- with pc $ \pc -> readIORef pc
    opcode <- getWord $ fromIntegral pc
    doInstruction opcode
    checkInteruptsFromDevices


------------------------------------------------------------------------------
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

makeSocket :: Maybe ConfigSocket -> IO (Maybe Socket)
makeSocket (Just (ConfigUnix a)) = do
    sock <- socket AF_UNIX Stream defaultProtocol
    Network.Socket.bind sock $ SockAddrUnix a
    Network.Socket.listen sock 1024
    return $ Just sock
makeSocket (Just (ConfigInet a)) = do
    sock <- socket AF_INET Stream defaultProtocol
    Network.Socket.bind sock $ SockAddrInet (read a) 0x0100007f
    Network.Socket.listen sock 1024
    return $ Just sock
makeSocket Nothing = return Nothing

makeMachine :: VM.IOVector Byte -> V.Vector Byte
            -> Maybe ConfigSocket -> Maybe ConfigSocket
            -> Maybe ConfigSocket -> Maybe ConfigSocket
            -> Maybe ConfigSocket -> Maybe ConfigSocket
            -> Maybe ConfigSocket -> Maybe ConfigSocket
            -> IO Machine
makeMachine ramData romData s0 s1 s2 s3 s4 s5 s6 s7 = do
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
    ms0 <- makeSocket s0
    ms1 <- makeSocket s1
    ms2 <- makeSocket s2
    ms3 <- makeSocket s3
    ms4 <- makeSocket s4
    ms5 <- makeSocket s5
    ms6 <- makeSocket s6
    ms7 <- makeSocket s7
    mc0 <- newIORef Nothing
    mc1 <- newIORef Nothing
    mc2 <- newIORef Nothing
    mc3 <- newIORef Nothing
    mc4 <- newIORef Nothing
    mc5 <- newIORef Nothing
    mc6 <- newIORef Nothing
    mc7 <- newIORef Nothing
    return $ Machine pc sr drs ars usp ssp ramData romData
                     ms0 ms1 ms2 ms3 ms4 ms5 ms6 ms7
                     mc0 mc1 mc2 mc3 mc4 mc5 mc6 mc7
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
suem (Config _ ramSize romPath s0 s1 s2 s3 s4 s5 s6 s7) = do
    romData <- B.readFile romPath
    ram <- VM.replicate ramSize 0
    m <- makeMachine ram (V.fromList $ B.unpack $ romData)
                     s0 s1 s2 s3 s4 s5 s6 s7
    runEmulator runMachine m
