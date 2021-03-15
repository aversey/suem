module Device where

import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Machine


data DeviceRequest = DeviceGetByte Long
                   | DeviceGetWord Long
                   | DeviceGetLong Long
                   | DeviceSetByte Long Byte
                   | DeviceSetWord Long Word
                   | DeviceSetLong Long Long

checkInteruptsFromDevices :: Emulator ()
checkInteruptsFromDevices = return ()
