-- This module describes the semantics of machine instructions.
module Instructions where

import Prelude hiding (Word)
import Machine
import Utils
import Data.IORef


doORICCR :: Emulator ()
doORICCR = error "ORICCR"

doORISR :: Emulator ()
doORISR = error "ORISR"

doORI :: Int -> Int -> Int -> Emulator ()
doORI _ _ _ = error "ORI"

doANDICCR :: Emulator ()
doANDICCR = error "ANDICCR"

doANDISR :: Emulator ()
doANDISR = error "ANDISR"

doANDI :: Int -> Int -> Int -> Emulator ()
doANDI _ _ _ = error "ANDI"

doSUBI :: Int -> Int -> Int -> Emulator ()
doSUBI _ _ _ = error "SUBI"

doADDI :: Int -> Int -> Int -> Emulator ()
doADDI _ _ _ = error "ADDI"

doEORICCR :: Emulator ()
doEORICCR = error "EORICCR"

doEORISR :: Emulator ()
doEORISR = error "EORISR"

doEORI :: Int -> Int -> Int -> Emulator ()
doEORI _ _ _ = error "EORI"

doCMPI :: Int -> Int -> Int -> Emulator ()
doCMPI _ _ _ = error "CMPI"

doMOVEP :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEP _ _ _ _ = error "MOVEP"

doBTST :: Int -> Int -> Int -> Int -> Emulator ()
doBTST _ _ _ _ = error "BTST"

doBCHG :: Int -> Int -> Int -> Int -> Emulator ()
doBCHG _ _ _ _ = error "BCHG"

doBCLR :: Int -> Int -> Int -> Int -> Emulator ()
doBCLR _ _ _ _ = error "BCLR"

doBSET :: Int -> Int -> Int -> Int -> Emulator ()
doBSET _ _ _ _ = error "BSET"

doMOVEA :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEA size dst_reg src_mode src_reg = do
    incPC
    (src_get, src_set) <- getOp src_mode src_reg (getMoveSize size)
    (dst_get, dst_set) <- getOp 1 dst_reg 4
    src_val <- src_get
    let val = signExtend src_val (getMoveSize size)
    dst_set val

doMOVE :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doMOVE size dst_reg dst_mode src_mode src_reg = do
    incPC
    (src_get, src_set) <- getOp src_mode src_reg (getMoveSize size)
    (dst_get, dst_set) <- getOp dst_mode dst_reg (getMoveSize size)
    src_val <- src_get
    dst_set src_val
    setNegative (checkNegative src_val (getMoveSize size))
    setZero (checkZero src_val)
    setOverflow False
    setCarry False

doSRMOVE :: Int -> Int -> Emulator ()
doSRMOVE _ _ = error "SRMOVE"

doMOVECCR :: Int -> Int -> Emulator ()
doMOVECCR _ _ = error "MOVECCR"

doMOVESR :: Int -> Int -> Emulator ()
doMOVESR _ _ = error "MOVESR"

doNEGX :: Int -> Int -> Int -> Emulator ()
doNEGX _ _ _ = error "NEGX"

doCLR :: Int -> Int -> Int -> Emulator ()
doCLR _ _ _ = error "CLR"

doNEG :: Int -> Int -> Int -> Emulator ()
doNEG _ _ _ = error "NEG"

doNOT :: Int -> Int -> Int -> Emulator ()
doNOT _ _ _ = error "NOT"

doEXT :: Int -> Int -> Emulator ()
doEXT _ _ = error "EXT"

doNBCD :: Int -> Int -> Emulator ()
doNBCD _ _ = error "NBCD"

doSWAP :: Int -> Emulator ()
doSWAP _ = error "SWAP"

doPEA :: Int -> Int -> Emulator ()
doPEA _ _ = error "PEA"

doILLEGAL :: Emulator ()
doILLEGAL = error "ILLEGAL"

doTAS :: Int -> Int -> Emulator ()
doTAS _ _ = error "TAS"

doTST :: Int -> Int -> Int -> Emulator ()
doTST _ _ _ = error "TST"

doTRAP :: Int -> Emulator ()
doTRAP _ = error "TRAP"

doLINK :: Int -> Emulator ()
doLINK a = do
    incPC
    addr <- readA a 4
    sp <- readA 7 4
    writeA 7 4 (sp - 4)
    setMemory (sp - 4) 4 addr

doUNLK :: Int -> Emulator ()
doUNLK a = do
    incPC
    addr <- readA a 4
    val <- getMemory addr 4
    writeA a 4 val
    writeA 7 4 (addr + 4)

doMOVEUSP :: Int -> Int -> Emulator ()
doMOVEUSP _ _ = error "MOVEUSP"

doRESET :: Emulator ()
doRESET = error "RESET"

doNOP :: Emulator ()
doNOP = do
    incPC

doSTOP :: Emulator ()
doSTOP = error "STOP"

doRTE :: Emulator ()
doRTE = error "RTE"

doRTS :: Emulator ()
doRTS = do
    sp <- readA 7 4
    writeA 7 4 (sp + 4)
    addr <- getMemory sp 4
    writePC addr

doTRAPV :: Emulator ()
doTRAPV = error "TRAPV"

doRTR :: Emulator ()
doRTR = error "RTR"

doJSR :: Int -> Int -> Emulator ()
doJSR _ _ = error "JSR"

doJMP :: Int -> Int -> Emulator ()
doJMP _ _ = error "JMP"

doMOVEM :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEM _ _ _ _ = error "MOVEM"

doLEA :: Int -> Int -> Int -> Emulator ()
doLEA _ _ _ = error "LEA"

doCHK :: Int -> Int -> Int -> Emulator ()
doCHK _ _ _ = error "CHK"

doDBcc :: Int -> Int -> Emulator ()
doDBcc _ _ = error "DBcc"

doScc :: Int -> Int -> Int -> Emulator ()
doScc _ _ _ = error "Scc"

doADDQ :: Int -> Int -> Int -> Int -> Emulator ()
doADDQ _ _ _ _ = error "ADDQ"

doSUBQ :: Int -> Int -> Int -> Int -> Emulator ()
doSUBQ _ _ _ _ = error "SUBQ"

doBSR :: Int -> Emulator ()
doBSR disp = do
    incPC
    pc <- readPC
    tmp_disp <- if disp == 0
        then getMemory pc 2
        else return $ fromIntegral disp
    let final_disp = signExtend tmp_disp (if disp == 0 then 2 else 1)
    writePC $ pc + final_disp
    let return_address = if disp == 0 then pc + 2 else pc
    sp <- readA 7 4
    writeA 7 4 (sp - 4)
    setMemory (sp - 4) 4 (fromIntegral return_address)

checkBccCondition :: Int -> Emulator Bool
-- BRA
checkBccCondition 0 = do
    return True
-- BNE
checkBccCondition 6 = do
    zf <- isZero
    return $ not zf
-- BEQ
checkBccCondition 7 = isZero

doBcc :: Int -> Int -> Emulator ()
doBcc cc disp = do
    incPC
    pc <- readPC
    check <- checkBccCondition cc
    tmp_disp <- if disp == 0
        then if check then getMemory pc 2 else return 2
        else if check then return $ fromIntegral disp else return 0
    let final_disp = signExtend tmp_disp (if disp == 0 then 2 else 1)
    writePC $ pc + final_disp

doMOVEQ :: Int -> Int -> Emulator ()
doMOVEQ _ _ = error "MOVEQ"

doDIVU :: Int -> Int -> Int -> Emulator ()
doDIVU _ _ _ = error "DIVU"

doDIVS :: Int -> Int -> Int -> Emulator ()
doDIVS _ _ _ = error "DIVS"

doSBCD :: Int -> Int -> Int -> Emulator ()
doSBCD _ _ _ = error "SBCD"

doOR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doOR _ _ _ _ _ = error "OR"

doSUBA :: Int -> Int -> Int -> Int -> Emulator ()
doSUBA _ _ _ _ = error "SUBA"

doSUBX :: Int -> Int -> Int -> Int -> Emulator ()
doSUBX _ _ _ _ = error "SUBX"

doSUB :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doSUB _ _ _ _ _ = error "SUB"

doCMPA :: Int -> Int -> Int -> Int -> Emulator ()
doCMPA _ _ _ _ = error "CMPA"

doCMP :: Int -> Int -> Int -> Int -> Emulator ()
doCMP _ _ _ _ = error "CMP"

doCMPM :: Int -> Int -> Int -> Emulator ()
doCMPM _ _ _ = error "CMPM"

doEOR :: Int -> Int -> Int -> Int -> Emulator ()
doEOR _ _ _ _ = error "EOR"

doMULU :: Int -> Int -> Int -> Emulator ()
doMULU _ _ _ = error "MULU"

doMULS :: Int -> Int -> Int -> Emulator ()
doMULS _ _ _ = error "MULS"

doABCD :: Int -> Int -> Int -> Emulator ()
doABCD _ _ _ = error "ABCD"

doEXG :: Int -> Int -> Int -> Int -> Emulator ()
doEXG _ _ _ _ = error "EXG"

doAND :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doAND _ _ _ _ _ = error "AND"

doADDA :: Int -> Int -> Int -> Int -> Emulator ()
doADDA dst_reg 0 src_mode src_reg = do
    incPC
    (src_get, src_set) <- getOp src_mode src_reg 4
    src_val <- src_get
    dst_val <- readA dst_reg 4
    let value = src_val + dst_val
    writeA dst_reg 4 value
    setNegative (checkNegative value 4)
    setZero (checkZero value)
--TODO flags
doADDA src_reg 1 dst_mode dst_reg = do
    incPC
    (dst_get, dst_set) <- getOp dst_mode dst_reg 4
    src_val <- readA src_reg 4
    dst_val <- dst_get
    let value = src_val + dst_val
    dst_set value
    setNegative (checkNegative value 4)
    setZero (checkZero value)
-- TODO flags

doADDX :: Int -> Int -> Int -> Int -> Emulator ()
doADDX _ _ _ _ = error "ADDX"

doADD :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doADD dst_reg 0 size src_mode src_reg = do
    incPC
    (src_get, src_set) <- getOp src_mode src_reg (getSize size)
    src_val <- src_get
    dst_val <- readD dst_reg (getSize size)
    let value = src_val + dst_val
    writeD dst_reg (getSize size) value
    setNegative (checkNegative value (getSize size))
    setZero (checkZero value)
-- TODO flags
doADD src_reg 1 size dst_mode dst_reg = do
    incPC
    (dst_get, dst_set) <- getOp dst_mode dst_reg (getSize size)
    src_val <- readD src_reg (getSize size)
    dst_val <- dst_get
    let value = src_val + dst_val
    dst_set value
    setNegative (checkNegative value (getSize size))
    setZero (checkZero value)
-- TODO flags

doASD :: Int -> Int -> Int -> Emulator ()
doASD _ _ _ = error "ASD"

doLSD :: Int -> Int -> Int -> Emulator ()
doLSD _ _ _ = error "LSD"

doROXd :: Int -> Int -> Int -> Emulator ()
doROXd _ _ _ = error "ROXd"

doROd :: Int -> Int -> Int -> Emulator ()
doROd _ _ _ = error "ROd"

doADSR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doADSR _ _ _ _ _ = error "ADSR"

doLSDR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doLSDR _ _ _ _ _ = error "LSDR"

doROXdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doROXdR _ _ _ _ _ = error "ROXdR"

doROdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doROdR _ _ _ _ _ = error "ROdR"
