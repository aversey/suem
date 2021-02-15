module Commands where

import Control.Lens
import Machine
import Utils

doNothing :: Machine -> Machine
doNothing m = Machine (Registers (pc r + 2) (sr r) (drs r)
                                 (ars r) (usp r) (ssp r))
                      (ram m) (rom m)
    where r = regs m

_doUnlink :: Int -> Machine -> Machine
_doUnlink 7 m = let r = regs m in if isSupervisor m
    then Machine (Registers (pc r + 2) (sr r) (drs r) (ars r)
                            (usp r) (getLong m (fromIntegral $ ssp r) + 4))
                 (ram m) (rom m)
    else Machine (Registers (pc r + 2) (sr r) (drs r) (ars r)
                            (getLong m (fromIntegral $ usp r) + 4) (ssp r))
                 (ram m) (rom m)
_doUnlink a m = let
        r = regs m
        av = getLong m (fromIntegral (ars r !! a))
        newars = ars r & element (fromIntegral a) .~ av
    in if isSupervisor m
    then Machine (Registers (pc r + 2) (sr r) (drs r)
                            newars (usp r) (av + 4))
                 (ram m) (rom m)
    else Machine (Registers (pc r + 2) (sr r) (drs r)
                            newars (av + 4) (ssp r))
                 (ram m) (rom m)
doUnlink :: [Int] -> Machine -> Machine
doUnlink = _doUnlink . fromBits

doReset :: Machine -> Machine
doReset = id

doStop :: Machine -> Machine
doStop = id

doRTE :: Machine -> Machine
doRTE = id

doRTS :: Machine -> Machine
doRTS = id

doTrapV :: Machine -> Machine
doTrapV = id

doRTR :: Machine -> Machine
doRTR = id

doIllegal :: Machine -> Machine
doIllegal = id

_doTAS :: Int -> Int -> Machine -> Machine
_doTAS _ _ = id
doTAS :: [Int] -> [Int] -> Machine -> Machine
doTAS = args2 _doTAS

_doTST :: Int -> Int -> Int -> Machine -> Machine
_doTST _ _ _ = id
doTST :: [Int] -> [Int] -> [Int] -> Machine -> Machine
doTST = args3 _doTST

_doTrap :: Int -> Machine -> Machine
_doTrap _ = id
doTrap :: [Int] -> Machine -> Machine
doTrap = _doTrap . fromBits

_doLink :: Int -> Machine -> Machine
_doLink _ = id
doLink :: [Int] -> Machine -> Machine
doLink = _doLink . fromBits
