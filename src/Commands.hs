-- This module describes the semantics of machine commands.
module Commands where

import Prelude hiding (Word)
import Machine
import Utils
import Data.IORef


doNothing :: Emulator ()
doNothing = with pc $ \pc -> do
    pcval <- readIORef pc
    writeIORef pc (pcval + 2)

_doUnlink :: Int -> Emulator ()
_doUnlink a = do
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
doUnlink :: [Int] -> Emulator ()
doUnlink = _doUnlink . fromBits

doReset :: Emulator ()
doReset = return ()

doStop :: Emulator ()
doStop = return ()

doRTE :: Emulator ()
doRTE = return ()

doRTS :: Emulator ()
doRTS = return ()

doTrapV :: Emulator ()
doTrapV = return ()

doRTR :: Emulator ()
doRTR = return ()

doIllegal :: Emulator ()
doIllegal = return ()

_doTAS :: Int -> Int -> Emulator ()
_doTAS _ _ = return ()
doTAS :: [Int] -> [Int] -> Emulator ()
doTAS = args2 _doTAS

_doTST :: Int -> Int -> Int -> Emulator ()
_doTST _ _ _ = return ()
doTST :: [Int] -> [Int] -> [Int] -> Emulator ()
doTST = args3 _doTST

_doTrap :: Int -> Emulator ()
_doTrap _ = return ()
doTrap :: [Int] -> Emulator ()
doTrap = _doTrap . fromBits

_doLink :: Int -> Emulator ()
_doLink _ = return ()
doLink :: [Int] -> Emulator ()
doLink = _doLink . fromBits
