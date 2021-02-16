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
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,0,0] = doReset
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,0,1] = doNothing
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,1,0] = doStop
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,0,1,1] = doRTE
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,0,1] = doRTS
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,1,0] = doTrapV
doCommand [0,1,0,0,1,1,1,0, 0,1,1,0,0,1,1,1] = doRTR
doCommand [0,1,0,0,1,0,1,0, 1,1,1,1,1,1,0,0] = doIllegal
doCommand [0,1,0,0,1,0,1,0, 1,1,a,b,c,d,e,f] = doTAS [a,b,c] [d,e,f]
doCommand [0,1,0,0,1,0,1,0, 0,1,0,0,a,b,c,d] = doTrap [a,b,c,d]
doCommand [0,1,0,0,1,0,1,0, 0,1,0,1,0,a,b,c] = doLink [a,b,c]
doCommand [0,1,0,0,1,0,1,0, 0,1,0,1,1,a,b,c] = doUnlink [a,b,c]
doCommand [0,1,0,0,1,0,1,0, a,b,c,d,e,f,g,h] = doTST [a,b] [c,d,e] [f,g,h]
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
