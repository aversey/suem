module Commands where

import Control.Lens
import Machine

doNothing :: Machine -> Machine
doNothing m = Machine (Registers (pc r + 2) (sr r) (drs r)
                                 (ars r) (usp r) (ssp r))
                      (ram m) (rom m)
    where r = regs m

doUnlink :: Int -> Machine -> Machine
doUnlink 7 m = let r = regs m in if isSupervisor m
    then Machine (Registers (pc r + 2) (sr r) (drs r) (ars r)
                            (usp r) (getLong m (fromIntegral $ ssp r) + 4))
                 (ram m) (rom m)
    else Machine (Registers (pc r + 2) (sr r) (drs r) (ars r)
                            (getLong m (fromIntegral $ usp r) + 4) (ssp r))
                 (ram m) (rom m)
doUnlink a m = let
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
