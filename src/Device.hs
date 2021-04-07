module Device where

import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Machine


-------------------------------------------------------------------------------
-- Protocol Constants
-- Protocol itself is simple,
-- suem send commands to devices, they response.
-- Example communication:
-- s: h afebabe
-- d: o 07
-- s: W afebabe 1999
-- d: o
-- d: i
-- s: l afebab3
-- d: x
-- All numbers are in hex and have correct length
-- (zeroes on the left are mandatory).

-- requests
device_get_high_byte = 'h' -- <7 hexes>; 2 hexes in response
device_get_low_byte  = 'l' -- <7 hexes>; 2 hexes in response
device_get_word      = 'w' -- <7 hexes>; 4 hexes in response

device_set_high_byte = 'H' -- <7 hexes> <2 hexes>
device_set_low_byte  = 'L' -- <7 hexes> <2 hexes>
device_set_word      = 'W' -- <7 hexes> <4 hexes>

-- responses
device_interrupt = 'i' -- and nothing else
device_ok        = 'o' -- maybe number in response to get (with correct size)
device_bad       = 'x' -- and nothing else =)


-------------------------------------------------------------------------------
-- Memory

deviceGetByte :: Long -> Emulator Byte
deviceGetByte a | a `mod` 2 == 0 = return 0xFF
                | otherwise = return 0xFF

deviceGetWord :: Long -> Emulator Word
deviceGetWord a = return 0xFFFF


deviceSetByte :: Long -> Byte -> Emulator ()
deviceSetByte a b | a `mod` 2 == 0 = return ()
                  | otherwise = return ()

deviceSetWord :: Long -> Word -> Emulator ()
deviceSetWord a w = return ()


-------------------------------------------------------------------------------
-- Interrupts

checkInteruptsFromDevices :: Emulator ()
checkInteruptsFromDevices = return ()
