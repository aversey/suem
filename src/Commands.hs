-- This module describes the semantics of machine commands.
module Commands where

import Prelude hiding (Word)
import Machine
import Utils
import Data.IORef


doORICCR :: Emulator ()
doORICCR = return ()

doORISR :: Emulator ()
doORISR = return ()

doORI :: Int -> Int -> Int -> Emulator ()
doORI _ _ _ = return ()

doANDICCR :: Emulator ()
doANDICCR = return ()

doANDISR :: Emulator ()
doANDISR = return ()

doANDI :: Int -> Int -> Int -> Emulator ()
doANDI _ _ _ = return ()

doSUBI :: Int -> Int -> Int -> Emulator ()
doSUBI _ _ _ = return ()

doADDI :: Int -> Int -> Int -> Emulator ()
doADDI _ _ _ = return ()

doEORICCR :: Emulator ()
doEORICCR = return ()

doEORISR :: Emulator ()
doEORISR = return ()

doEORI :: Int -> Int -> Int -> Emulator ()
doEORI _ _ _ = return ()

doCMPI :: Int -> Int -> Int -> Emulator ()
doCMPI _ _ _ = return ()

doMOVEP :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEP _ _ _ _ = return ()

doBTST :: Int -> Int -> Int -> Int -> Emulator ()
doBTST _ _ _ _ = return ()

doBCHG :: Int -> Int -> Int -> Int -> Emulator ()
doBCHG _ _ _ _ = return ()

doBCLR :: Int -> Int -> Int -> Int -> Emulator ()
doBCLR _ _ _ _ = return ()

doBSET :: Int -> Int -> Int -> Int -> Emulator ()
doBSET _ _ _ _ = return ()

doMOVEA :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEA _ _ _ _ = return ()

doMOVE :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doMOVE _ _ _ _ _ = return ()

doSRMOVE :: Int -> Int -> Emulator ()
doSRMOVE _ _ = return ()

doMOVECCR :: Int -> Int -> Emulator ()
doMOVECCR _ _ = return ()

doMOVESR :: Int -> Int -> Emulator ()
doMOVESR _ _ = return ()

doNEGX :: Int -> Int -> Int -> Emulator ()
doNEGX _ _ _ = return ()

doCLR :: Int -> Int -> Int -> Emulator ()
doCLR _ _ _ = return ()

doNEG :: Int -> Int -> Int -> Emulator ()
doNEG _ _ _ = return ()

doNOT :: Int -> Int -> Int -> Emulator ()
doNOT _ _ _ = return ()

doEXT :: Int -> Int -> Emulator ()
doEXT _ _ = return ()

doNBCD :: Int -> Int -> Emulator ()
doNBCD _ _ = return ()

doSWAP :: Int -> Emulator ()
doSWAP _ = return ()

doPEA :: Int -> Int -> Emulator ()
doPEA _ _ = return ()

doILLEGAL :: Emulator ()
doILLEGAL = return ()

doTAS :: Int -> Int -> Emulator ()
doTAS _ _ = return ()

doTST :: Int -> Int -> Int -> Emulator ()
doTST _ _ _ = return ()

doTRAP :: Int -> Emulator ()
doTRAP _ = return ()

doLINK :: Int -> Emulator ()
doLINK _ = return ()

doUNLK :: Int -> Emulator ()
doUNLK a = do
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

doMOVEUSP :: Int -> Int -> Emulator ()
doMOVEUSP _ _ = return ()

doRESET :: Emulator ()
doRESET = return ()

doNOP :: Emulator ()
doNOP = with pc $ \pc -> do
    pcval <- readIORef pc
    writeIORef pc (pcval + 2)

doSTOP :: Emulator ()
doSTOP = return ()

doRTE :: Emulator ()
doRTE = return ()

doRTS :: Emulator ()
doRTS = return ()

doTRAPV :: Emulator ()
doTRAPV = return ()

doRTR :: Emulator ()
doRTR = return ()

doJSR :: Int -> Int -> Emulator ()
doJSR _ _ = return ()

doJMP :: Int -> Int -> Emulator ()
doJMP _ _ = return ()

doMOVEM :: Int -> Int -> Int -> Int -> Emulator ()
doMOVEM _ _ _ _ = return ()

doLEA :: Int -> Int -> Int -> Emulator ()
doLEA _ _ _ = return ()

doCHK :: Int -> Int -> Int -> Emulator ()
doCHK _ _ _ = return ()

doDBcc :: Int -> Int -> Emulator ()
doDBcc _ _ = return ()

doScc :: Int -> Int -> Int -> Emulator ()
doScc _ _ _ = return ()

doADDQ :: Int -> Int -> Int -> Int -> Emulator ()
doADDQ _ _ _ _ = return ()

doSUBQ :: Int -> Int -> Int -> Int -> Emulator ()
doSUBQ _ _ _ _ = return ()

doBRA :: Int -> Emulator ()
doBRA _ = return ()

doBSR :: Int -> Emulator ()
doBSR _ = return ()

doBcc :: Int -> Int -> Emulator ()
doBcc _ _ = return ()

doMOVEQ :: Int -> Int -> Emulator ()
doMOVEQ _ _ = return ()

doDIVU :: Int -> Int -> Int -> Emulator ()
doDIVU _ _ _ = return ()

doDIVS :: Int -> Int -> Int -> Emulator ()
doDIVS _ _ _ = return ()

doSBCD :: Int -> Int -> Int -> Emulator ()
doSBCD _ _ _ = return ()

doOR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doOR _ _ _ _ _ = return ()

doSUBA :: Int -> Int -> Int -> Int -> Emulator ()
doSUBA _ _ _ _ = return ()

doSUBX :: Int -> Int -> Int -> Int -> Emulator ()
doSUBX _ _ _ _ = return ()

doSUB :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doSUB _ _ _ _ _ = return ()

doCMPA :: Int -> Int -> Int -> Int -> Emulator ()
doCMPA _ _ _ _ = return ()

doCMP :: Int -> Int -> Int -> Int -> Emulator ()
doCMP _ _ _ _ = return ()

doCMPM :: Int -> Int -> Int -> Emulator ()
doCMPM _ _ _ = return ()

doEOR :: Int -> Int -> Int -> Int -> Emulator ()
doEOR _ _ _ _ = return ()

doMULU :: Int -> Int -> Int -> Emulator ()
doMULU _ _ _ = return ()

doMULS :: Int -> Int -> Int -> Emulator ()
doMULS _ _ _ = return ()

doABCD :: Int -> Int -> Int -> Emulator ()
doABCD _ _ _ = return ()

doEXG :: Int -> Int -> Int -> Int -> Emulator ()
doEXG _ _ _ _ = return ()

doAND :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doAND _ _ _ _ _ = return ()

doADDA :: Int -> Int -> Int -> Int -> Emulator ()
doADDA _ _ _ _ = return ()

doADDX :: Int -> Int -> Int -> Int -> Emulator ()
doADDX _ _ _ _ = return ()

doADD :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doADD _ _ _ _ _ = return ()

doASD :: Int -> Int -> Int -> Emulator ()
doASD _ _ _ = return ()

doLSD :: Int -> Int -> Int -> Emulator ()
doLSD _ _ _ = return ()

doROXd :: Int -> Int -> Int -> Emulator ()
doROXd _ _ _ = return ()

doROd :: Int -> Int -> Int -> Emulator ()
doROd _ _ _ = return ()

doADSR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doADSR _ _ _ _ _ = return ()

doLSDR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doLSDR _ _ _ _ _ = return ()

doROXdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doROXdR _ _ _ _ _ = return ()

doROdR :: Int -> Int -> Int -> Int -> Int -> Emulator ()
doROdR _ _ _ _ _ = return ()
