-- This module describes the semantics of machine commands.
module Commands where

import Prelude hiding (Word)
import Machine
import Utils
import Data.IORef

-- ORICCR
doORICCR :: Emulator ()
doORICCR = return () 

-- ORISR
doORISR :: Emulator ()
doORISR = return ()

-- ORI
_doORI :: Int -> Int -> Int -> Emulator ()
_doORI _ _ _ = return ()
doORI :: [Int] -> [Int] -> [Int] -> Emulator ()
doORI = args3 _doORI

-- ANDICCR
doANDICCR :: Emulator ()
doANDICCR = return ()

-- ANDISR
doANDISR :: Emulator ()
doANDISR = return ()

-- ANDI
_doANDI :: Int -> Int -> Int -> Emulator ()
_doANDI _ _ _ = return ()
doANDI :: [Int] -> [Int] -> [Int] -> Emulator ()
doANDI = args3 _doANDI

-- SUBI
_doSUBI :: Int -> Int -> Int -> Emulator ()
_doSUBI _ _ _ = return ()
doSUBI :: [Int] -> [Int] -> [Int] -> Emulator ()
doSUBI = args3 _doSUBI

-- ADDI
_doADDI :: Int -> Int -> Int -> Emulator ()
_doADDI _ _ _ = return ()
doADDI :: [Int] -> [Int] -> [Int] -> Emulator ()
doADDI = args3 _doADDI

-- EORICCR
doEORICCR :: Emulator ()
doEORICCR = return ()

-- EORISR
doEORISR :: Emulator ()
doEORISR = return ()

-- EORI
_doEORI :: Int -> Int -> Int -> Emulator ()
_doEORI _ _ _ = return ()
doEORI :: [Int] -> [Int] -> [Int] -> Emulator ()
doEORI = args3 _doEORI

-- CMPI
_doCMPI :: Int -> Int -> Int -> Emulator ()
_doCMPI _ _ _ = return ()
doCMPI :: [Int] -> [Int] -> [Int] -> Emulator ()
doCMPI = args3 _doCMPI

-- MOVEP
_doMOVEP :: Int -> Int -> Int -> Int -> Emulator ()
_doMOVEP _ _ _ _ = return ()
doMOVEP :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doMOVEP = args4 _doMOVEP

-- BTST
_doBTST :: Int -> Int -> Int -> Int -> Emulator ()
_doBTST _ _ _ _ = return ()
doBTST :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doBTST = args4 _doBTST

-- BCHG
_doBCHG :: Int -> Int -> Int -> Int -> Emulator ()
_doBCHG _ _ _ _ = return ()
doBCHG :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doBCHG = args4 _doBCHG

-- BCLR
_doBCLR :: Int -> Int -> Int -> Int -> Emulator ()
_doBCLR _ _ _ _ = return ()
doBCLR :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doBCLR = args4 _doBCLR

-- BSET
_doBSET :: Int -> Int -> Int -> Int -> Emulator ()
_doBSET _ _ _ _ = return ()
doBSET :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doBSET = args4 _doBSET

-- MOVEA
_doMOVEA :: Int -> Int -> Int -> Int -> Emulator ()
_doMOVEA _ _ _ _ = return ()
doMOVEA :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doMOVEA = args4 _doMOVEA

-- MOVE
_doMOVE :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doMOVE _ _ _ _ _ = return ()
doMOVE :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doMOVE = args5 _doMOVE

-- SRMOVE
_doSRMOVE :: Int -> Int -> Emulator ()
_doSRMOVE _ _ = return ()
doSRMOVE :: [Int] -> [Int] -> Emulator ()
doSRMOVE = args2 _doSRMOVE

-- MOVECCR
_doMOVECCR :: Int -> Int -> Emulator ()
_doMOVECCR _ _ = return ()
doMOVECCR :: [Int] -> [Int] -> Emulator ()
doMOVECCR = args2 _doMOVECCR

-- MOVESR
_doMOVESR :: Int -> Int -> Emulator ()
_doMOVESR _ _ = return ()
doMOVESR :: [Int] -> [Int] -> Emulator ()
doMOVESR = args2 _doMOVESR

-- NEGX
_doNEGX :: Int -> Int -> Int -> Emulator ()
_doNEGX _ _ _ = return ()
doNEGX :: [Int] -> [Int] -> [Int] -> Emulator ()
doNEGX = args3 _doNEGX

-- CLR
_doCLR :: Int -> Int -> Int -> Emulator ()
_doCLR _ _ _ = return ()
doCLR :: [Int] -> [Int] -> [Int] -> Emulator ()
doCLR = args3 _doCLR

-- NEG
_doNEG :: Int -> Int -> Int -> Emulator ()
_doNEG _ _ _ = return ()
doNEG :: [Int] -> [Int] -> [Int] -> Emulator ()
doNEG = args3 _doNEG

-- NOT
_doNOT :: Int -> Int -> Int -> Emulator ()
_doNOT _ _ _ = return ()
doNOT :: [Int] -> [Int] -> [Int] -> Emulator ()
doNOT = args3 _doNOT

-- EXT
_doEXT :: Int -> Int -> Emulator ()
_doEXT _ _ = return ()
doEXT :: [Int] -> [Int] -> Emulator ()
doEXT = args2 _doEXT

-- NBCD
_doNBCD :: Int -> Int -> Emulator ()
_doNBCD _ _ = return ()
doNBCD :: [Int] -> [Int] -> Emulator ()
doNBCD = args2 _doNBCD

-- SWAP
_doSWAP :: Int -> Emulator ()
_doSWAP _ = return ()
doSWAP :: [Int] -> Emulator ()
doSWAP = _doSWAP . fromBits

-- PEA
_doPEA :: Int -> Int -> Emulator ()
_doPEA _ _ = return () 
doPEA :: [Int] -> [Int] -> Emulator ()
doPEA = args2 _doPEA

-- ILLEGAL
doILLEGAL :: Emulator ()
doILLEGAL = return ()

-- TAS
_doTAS :: Int -> Int -> Emulator ()
_doTAS _ _ = return ()
doTAS :: [Int] -> [Int] -> Emulator ()
doTAS = args2 _doTAS

-- TST
_doTST :: Int -> Int -> Int -> Emulator ()
_doTST _ _ _ = return ()
doTST :: [Int] -> [Int] -> [Int] -> Emulator ()
doTST = args3 _doTST

-- TRAP
_doTRAP :: Int -> Emulator ()
_doTRAP _ = return ()
doTRAP :: [Int] -> Emulator ()
doTRAP = _doTRAP . fromBits

-- LINK
_doLINK :: Int -> Emulator ()
_doLINK _ = return ()
doLINK :: [Int] -> Emulator ()
doLINK = _doLINK . fromBits

-- UNLK
_doUNLK :: Int -> Emulator ()
_doUNLK a = do
    addr <- readA a
    val <- getLong addr
    with pc $ \pc -> do
        pcval <- readIORef pc
        writeIORef pc (pcval + 2)
    isSupervisor >>= \sup -> if sup
    then with ssp $ \sp -> do
        writeIORef sp (val + 4)
    else with usp $ \sp -> do
        writeIORef sp (val + 4)
doUNLK :: [Int] -> Emulator ()
doUNLK = _doUNLK . fromBits

-- MOVEUSP
_doMOVEUSP :: Int -> Int -> Emulator ()
_doMOVEUSP _ _ = return ()
doMOVEUSP :: [Int] -> [Int] -> Emulator ()
doMOVEUSP = args2 _doMOVEUSP

-- RESET
doRESET :: Emulator ()
doRESET = return ()

-- NOP
doNOP :: Emulator ()
doNOP = with pc $ \pc -> do
    pcval <- readIORef pc
    writeIORef pc (pcval + 2)

-- STOP
doSTOP :: Emulator ()
doSTOP = return ()

-- RTE
doRTE :: Emulator ()
doRTE = return ()

-- RTS
doRTS :: Emulator ()
doRTS = return ()

-- TRAPV
doTRAPV :: Emulator ()
doTRAPV = return ()

-- RTR
doRTR :: Emulator ()
doRTR = return ()

-- JSR
_doJSR :: Int -> Int -> Emulator ()
_doJSR _ _ = return () 
doJSR :: [Int] -> [Int] -> Emulator ()
doJSR = args2 _doJSR

-- JMP
_doJMP :: Int -> Int -> Emulator ()
_doJMP _ _ = return ()
doJMP :: [Int] -> [Int] -> Emulator ()
doJMP = args2 _doJMP

-- MOVEM
_doMOVEM :: Int -> Int -> Int -> Int -> Emulator ()
_doMOVEM _ _ _ _ = return ()
doMOVEM :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doMOVEM = args4 _doMOVEM

-- LEA
_doLEA :: Int -> Int -> Int -> Emulator ()
_doLEA _ _ _ = return ()
doLEA :: [Int] -> [Int] -> [Int] -> Emulator ()
doLEA = args3 _doLEA

-- CHK
_doCHK :: Int -> Int -> Int -> Emulator ()
_doCHK _ _ _ = return ()
doCHK :: [Int] -> [Int] -> [Int] -> Emulator ()
doCHK = args3 _doCHK

-- DBcc
_doDBcc :: Int -> Int -> Emulator ()
_doDBcc _ _ = return ()
doDBcc :: [Int] -> [Int] -> Emulator ()
doDBcc = args2 _doDBcc

-- Scc
_doScc :: Int -> Int -> Int -> Emulator ()
_doScc _ _ _ = return ()
doScc :: [Int] -> [Int] -> [Int] -> Emulator ()
doScc = args3 _doScc

-- ADDQ
_doADDQ :: Int -> Int -> Int -> Int -> Emulator ()
_doADDQ _ _ _ _ = return ()
doADDQ :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doADDQ = args4 _doADDQ

-- SUBQ
_doSUBQ :: Int -> Int -> Int -> Int -> Emulator ()
_doSUBQ _ _ _ _ = return ()
doSUBQ :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doSUBQ = args4 _doSUBQ

-- BRA
_doBRA :: Int -> Emulator ()
_doBRA _ = return ()
doBRA :: [Int] -> Emulator ()
doBRA = _doBRA . fromBits

-- BSR
_doBSR :: Int -> Emulator ()
_doBSR _ = return ()
doBSR :: [Int] -> Emulator ()
doBSR = _doBSR . fromBits

-- Bcc
_doBcc :: Int -> Int -> Emulator ()
_doBcc _ _ = return () 
doBcc :: [Int] -> [Int] -> Emulator ()
doBcc = args2 _doBcc

-- MOVEQ
_doMOVEQ :: Int -> Int -> Emulator ()
_doMOVEQ _ _ = return ()
doMOVEQ :: [Int] -> [Int] -> Emulator ()
doMOVEQ = args2 _doMOVEQ

-- DIVU
_doDIVU :: Int -> Int -> Int -> Emulator ()
_doDIVU _ _ _ = return ()
doDIVU :: [Int] -> [Int] -> [Int] -> Emulator ()
doDIVU = args3 _doDIVU

-- DIVS
_doDIVS :: Int -> Int -> Int -> Emulator ()
_doDIVS _ _ _ = return ()
doDIVS :: [Int] -> [Int] -> [Int] -> Emulator ()
doDIVS = args3 _doDIVS

-- SBCD
_doSBCD :: Int -> Int -> Int -> Emulator ()
_doSBCD _ _ _ = return ()
doSBCD :: [Int] -> [Int] -> [Int] -> Emulator ()
doSBCD = args3 _doSBCD

-- OR
_doOR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doOR _ _ _ _ _ = return ()
doOR :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doOR = args5 _doOR

-- SUBA
_doSUBA :: Int -> Int -> Int -> Int -> Emulator ()
_doSUBA _ _ _ _ = return ()
doSUBA :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doSUBA = args4 _doSUBA

-- SUBX
_doSUBX :: Int -> Int -> Int -> Int -> Emulator ()
_doSUBX _ _ _ _ = return ()
doSUBX :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doSUBX = args4 _doSUBX

-- SUB
_doSUB :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doSUB _ _ _ _ _ = return ()
doSUB :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doSUB = args5 _doSUB

-- CMPA
_doCMPA :: Int -> Int -> Int -> Int -> Emulator ()
_doCMPA _ _ _ _ = return ()
doCMPA :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doCMPA = args4 _doCMPA

-- CMP
_doCMP :: Int -> Int -> Int -> Int -> Emulator ()
_doCMP _ _ _ _ = return ()
doCMP :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doCMP = args4 _doCMP

-- CMPM
_doCMPM :: Int -> Int -> Int -> Emulator ()
_doCMPM _ _ _ = return () 
doCMPM :: [Int] -> [Int] -> [Int] -> Emulator ()
doCMPM = args3 _doCMPM 

-- EOR
_doEOR :: Int -> Int -> Int -> Int -> Emulator ()
_doEOR _ _ _ _ = return ()
doEOR :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doEOR = args4 _doEOR

-- MULU
_doMULU :: Int -> Int -> Int -> Emulator ()
_doMULU _ _ _ = return ()
doMULU :: [Int] -> [Int] -> [Int] -> Emulator ()
doMULU = args3 _doMULU

-- MULS
_doMULS :: Int -> Int -> Int -> Emulator ()
_doMULS _ _ _ = return ()
doMULS :: [Int] -> [Int] -> [Int] -> Emulator ()
doMULS = args3 _doMULS

-- ABCD
_doABCD :: Int -> Int -> Int -> Emulator ()
_doABCD _ _ _ = return ()
doABCD :: [Int] -> [Int] -> [Int] -> Emulator ()
doABCD = args3 _doABCD

-- EXG
_doEXG :: Int -> Int -> Int -> Int -> Emulator ()
_doEXG _ _ _ _ = return ()
doEXG :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doEXG = args4 _doEXG

-- AND
_doAND :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doAND _ _ _ _ _ = return ()
doAND :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doAND = args5 _doAND

-- ADDA
_doADDA :: Int -> Int -> Int -> Int -> Emulator ()
_doADDA _ _ _ _ = return ()
doADDA :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doADDA = args4 _doADDA

-- ADDX
_doADDX :: Int -> Int -> Int -> Int -> Emulator ()
_doADDX _ _ _ _ = return () 
doADDX :: [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doADDX = args4 _doADDX

-- ADD
_doADD :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doADD _ _ _ _ _ = return () 
doADD :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doADD = args5 _doADD

-- ASD
_doASD :: Int -> Int -> Int -> Emulator ()
_doASD _ _ _ = return ()
doASD :: [Int] -> [Int] -> [Int] -> Emulator ()
doASD = args3 _doASD

-- LSD
_doLSD :: Int -> Int -> Int -> Emulator ()
_doLSD _ _ _ = return ()
doLSD :: [Int] -> [Int] -> [Int] -> Emulator ()
doLSD = args3 _doLSD

-- ROXd
_doROXd :: Int -> Int -> Int -> Emulator ()
_doROXd _ _ _ = return ()
doROXd :: [Int] -> [Int] -> [Int] -> Emulator ()
doROXd = args3 _doROXd

-- ROd
_doROd :: Int -> Int -> Int -> Emulator ()
_doROd _ _ _ = return ()
doROd :: [Int] -> [Int] -> [Int] -> Emulator ()
doROd = args3 _doROd 

-- ADSR
_doADSR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doADSR _ _ _ _ _ = return ()
doADSR :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doADSR = args5 _doADSR

-- LSDR
_doLSDR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doLSDR _ _ _ _ _ = return () 
doLSDR :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doLSDR = args5 _doLSDR

-- ROXdR
_doROXdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doROXdR _ _ _ _ _ = return ()
doROXdR :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doROXdR = args5 _doROXdR

-- ROdR
_doROdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
_doROdR _ _ _ _ _ = return ()
doROdR :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Emulator ()
doROdR = args5 _doROdR
