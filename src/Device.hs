module Machine where

import Data.Word (Word32, Word16, Word8)


data DeviceRequest = DeviceGetByte Long
                   | DeviceGetWord Long
                   | DeviceGetLong Long
                   | DeviceSetByte Long Word8
                   | DeviceSetWord Long Word16
                   | DeviceSetLong Long Word32

checkInteruptsFromDevices :: Emulator ()
checkInteruptsFromDevices = putStrLn "blah-blah"
