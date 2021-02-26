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

doCommand :: [Int] -> Emulator ()
doCommand [0,0,0,0,0,0,0,0, 0,0,1,1,1,1,0,0] = doORICCR
doCommand [0,0,0,0,0,0,0,0, 0,1,1,1,1,1,0,0] = doORISR
doCommand [0,0,0,0,0,0,0,0, i,j,a,b,c,x,y,z] = doORI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,0,0,1,0, 0,0,1,1,1,1,0,0] = doANDICCR
doCommand [0,0,0,0,0,0,1,0, 0,1,1,1,1,1,0,0] = doANDISR
doCommand [0,0,0,0,0,0,1,0, i,j,a,b,c,x,y,z] = doANDI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,0,1,0,0, i,j,a,b,c,x,y,z] = doSUBI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,0,1,1,0, i,j,a,b,c,x,y,z] = doADDI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,1,0,1,0, 0,0,1,1,1,1,0,0] = doEORICCR
doCommand [0,0,0,0,1,0,1,0, 0,1,1,1,1,1,0,0] = doEORISR
doCommand [0,0,0,0,1,0,1,0, i,j,a,b,c,x,y,z] = doEORI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,1,1,0,0, i,j,a,b,c,x,y,z] = doCMPI [i,j] [a,b,c] [x,y,z]
doCommand [0,0,0,0,u,v,w,1, t,i,0,0,1,x,y,z] = doMOVEP [u,v,w] [t] [i] [x,y,z]
doCommand [0,0,0,0,u,v,w,t, 0,0,a,b,c,x,y,z] = doBTST [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [0,0,0,0,u,v,w,t, 0,1,a,b,c,x,y,z] = doBCHG [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [0,0,0,0,u,v,w,t, 1,0,a,b,c,x,y,z] = doBCLR [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [0,0,0,0,u,v,w,t, 1,1,a,b,c,x,y,z] = doBSET [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [0,0,i,j,u,v,w,0, 0,1,a,b,c,x,y,z] = doMOVEA [i,j] [u,v,w] [a,b,c] [x,y,z]
doCommand [0,0,i,j,u,v,w,d, e,f,a,b,c,x,y,z] = doMOVE [i,j] [d,e,f] [u,v,w] [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,0,0,0, 1,1,a,b,c,x,y,z] = doSRMOVE [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,1,0,0, 1,1,a,b,c,x,y,z] = doMOVECCR [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,1,1,0, 1,1,a,b,c,x,y,z] = doMOVESR [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,0,0,0, i,j,a,b,c,x,y,z] = doNEGX [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,0,1,0, i,j,a,b,c,x,y,z] = doCLR [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,1,0,0, i,j,a,b,c,x,y,z] = doNEG [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,0,0,1,1,0, i,j,a,b,c,x,y,z] = doNOT [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,0,0,0, 1,i,0,0,0,x,y,z] = doEXT [i] [x,y,z]
doCommand [0,1,0,0,1,0,0,0, 0,0,a,b,c,x,y,z] = doNBCD [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,0,0,0, 0,1,0,0,0,x,y,z] = doSWAP [x,y,z]
doCommand [0,1,0,0,1,0,0,0, 0,1,a,b,c,x,y,z] = doPEA [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,0,1,0, 1,1,1,1,1,1,0,0] = doILLEGAL
doCommand [0,1,0,0,1,0,1,0, 1,1,a,b,c,x,y,z] = doTAS [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,0,1,0, i,j,a,b,c,x,y,z] = doTST [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,1,1,0, 0,1,0,0,a,b,c,d] = doTRAP [a,b,c,d]
doCommand [0,1,0,0,1,1,1,0, 0,1,0,1,0,x,y,z] = doLINK [x,y,z]
doCommand [0,1,0,0,1,1,1,0, 0,1,0,1,1,x,y,z] = doUNLK [x,y,z]
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,t,x,y,z] = doMOVEUSP [t] [x,y,z]
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,0,0,0] = doRESET
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,0,0,1] = doNOP
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,0,1,0] = doSTOP
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,0,1,1] = doRTE
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,1,0,1] = doRTS
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,1,1,0] = doTRAPV
doCommand [0,1,0,0,1,1,1,0, 0,1,1,1,0,1,1,1] = doRTR
doCommand [0,1,0,0,1,1,1,0, 1,0,a,b,c,x,y,z] = doJSR [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,1,1,0, 1,1,a,b,c,x,y,z] = doJMP [a,b,c] [x,y,z]
doCommand [0,1,0,0,1,t,0,0, 1,i,a,b,c,x,y,z] = doMOVEM [t] [i] [a,b,c] [x,y,z]
doCommand [0,1,0,0,u,v,w,1, 1,1,a,b,c,x,y,z] = doLEA [u,v,w] [a,b,c] [x,y,z]
doCommand [0,1,0,0,u,v,w,1, 1,0,a,b,c,x,y,z] = doCHK [u,v,w] [a,b,c] [x,y,z]
doCommand [0,1,0,1,u,v,w,t, 1,1,0,0,1,x,y,z] = doDBcc [u,v,w,t] [x,y,z]
doCommand [0,1,0,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doScc [u,v,w,t] [a,b,c] [x,y,z]
doCommand [0,1,0,1,u,v,w,0, i,j,a,b,c,x,y,z] = doADDQ [u,v,w] [i,j] [a,b,c] [x,y,z]
doCommand [0,1,0,1,u,v,w,1, i,j,a,b,c,x,y,z] = doSUBQ [u,v,w] [i,j] [a,b,c] [x,y,z]
doCommand [0,1,1,0,0,0,0,0, a,b,c,d,e,f,g,h] = doBRA [a,b,c,d,e,f,g,h]
doCommand [0,1,1,0,0,0,0,1, a,b,c,d,e,f,g,h] = doBSR [a,b,c,d,e,f,g,h]
doCommand [0,1,1,0,u,v,w,t, a,b,c,d,e,f,g,h] = doBcc [u,v,w,t] [a,b,c,d,e,f,g,h]
doCommand [0,1,1,1,u,v,w,0, a,b,c,d,e,f,g,h] = doMOVEQ [u,v,w] [a,b,c,d,e,f,g,h]
doCommand [1,0,0,0,u,v,w,0, 1,1,a,b,c,x,y,z] = doDIVU [u,v,w] [a,b,c] [x,y,z]
doCommand [1,0,0,0,u,v,w,1, 1,1,a,b,c,x,y,z] = doDIVS [u,v,w] [a,b,c] [x,y,z]
doCommand [1,0,0,0,u,v,w,1, 0,0,0,0,t,x,y,z] = doSBCD [u,v,w] [t] [x,y,z]
doCommand [1,0,0,0,u,v,w,t, i,j,a,b,c,x,y,z] = doOR [u,v,w] [t] [i,j] [a,b,c] [x,y,z]
doCommand [1,0,0,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doSUBA [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [1,0,0,1,u,v,w,1, i,j,0,0,t,x,y,z] = doSUBX [u,v,w] [i,j] [t] [x,y,z]
doCommand [1,0,0,1,u,v,w,t, i,j,a,b,c,x,y,z] = doSUB [u,v,w] [t] [i,j] [a,b,c] [x,y,z]
doCommand [1,0,1,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doCMPA [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [1,0,1,1,u,v,w,0, i,j,a,b,c,x,y,z] = doCMP [u,v,w] [i,j] [a,b,c] [x,y,z]
doCommand [1,0,1,1,u,v,w,1, i,j,0,0,1,x,y,z] = doCMPM [u,v,w] [i,j] [x,y,z]
doCommand [1,0,1,1,u,v,w,1, i,j,a,b,c,x,y,z] = doEOR [u,v,w] [i,j] [a,b,c] [x,y,z]
doCommand [1,1,0,0,u,v,w,0, 1,1,a,b,c,x,y,z] = doMULU [u,v,w] [a,b,c] [x,y,z]
doCommand [1,1,0,0,u,v,w,1, 1,1,a,b,c,x,y,z] = doMULS [u,v,w] [a,b,c] [x,y,z]
doCommand [1,1,0,0,u,v,w,1, 0,0,0,0,t,x,y,z] = doABCD [u,v,w] [t] [x,y,z]
doCommand [1,1,0,0,u,v,w,1, i,j,0,0,t,x,y,z] = doEXG [u,v,w] [i,j] [t] [x,y,z]
doCommand [1,1,0,0,u,v,w,t, i,j,a,b,c,x,y,z] = doAND [u,v,w] [t] [i,j] [a,b,c] [x,y,z]
doCommand [1,1,0,1,u,v,w,t, 1,1,a,b,c,x,y,z] = doADDA [u,v,w] [t] [a,b,c] [x,y,z]
doCommand [1,1,0,1,u,v,w,1, i,j,0,0,t,x,y,z] = doADDX [u,v,w] [i,j] [t] [x,y,z]
doCommand [1,1,0,1,u,v,w,t, i,j,a,b,c,x,y,z] = doADD [u,v,w] [t] [i,j] [a,b,c] [x,y,z]
doCommand [1,1,1,0,0,0,0,t, 1,1,a,b,c,x,y,z] = doASD [t] [a,b,c] [x,y,z]
doCommand [1,1,1,0,0,0,1,t, 1,1,a,b,c,x,y,z] = doLSD [t] [a,b,c] [x,y,z]
doCommand [1,1,1,0,0,1,0,t, 1,1,a,b,c,x,y,z] = doROXd [t] [a,b,c] [x,y,z]
doCommand [1,1,1,0,0,1,1,t, 1,1,a,b,c,x,y,z] = doROd [t] [a,b,c] [x,y,z]
doCommand [1,1,1,0,u,v,w,t, i,j,a,0,0,x,y,z] = doADSR [u,v,w] [t] [i,j] [a] [x,y,z]
doCommand [1,1,1,0,u,v,w,t, i,j,a,0,1,x,y,z] = doLSDR [u,v,w] [t] [i,j] [a] [x,y,z]
doCommand [1,1,1,0,u,v,w,t, i,j,a,1,0,x,y,z] = doROXdR [u,v,w] [t] [i,j] [a] [x,y,z]
doCommand [1,1,1,0,u,v,w,t, i,j,a,1,1,x,y,z] = doROdR [u,v,w] [t] [i,j] [a] [x,y,z]
doCommand _ = error "Bad command."

runMachine :: Emulator ()
runMachine = forM_ [0..] $ \_ -> do
    pc <- with pc $ \pc -> readIORef pc
    cmd <- getWord $ fromIntegral pc
    doCommand (toBitsWhole cmd)


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
