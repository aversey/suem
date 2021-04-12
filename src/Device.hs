{-# LANGUAGE BinaryLiterals #-}
module Device where

import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Data.IORef
import Data.Char
import Data.List
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import System.IO
import Data.String
import Data.ByteString
import Machine
import Network.Socket


getSock :: Long -> Emulator (Maybe Handle)
getSock a | a < 0x900000 = do { m <- ask; liftIO $ sock (c0 m) (s0 m) }
          | a < 0xa00000 = do { m <- ask; liftIO $ sock (c1 m) (s1 m) }
          | a < 0xb00000 = do { m <- ask; liftIO $ sock (c2 m) (s2 m) }
          | a < 0xc00000 = do { m <- ask; liftIO $ sock (c3 m) (s3 m) }
          | a < 0xd00000 = do { m <- ask; liftIO $ sock (c4 m) (s4 m) }
          | a < 0xe00000 = do { m <- ask; liftIO $ sock (c5 m) (s5 m) }
          | a < 0xf00000 = do { m <- ask; liftIO $ sock (c6 m) (s6 m) }
          | otherwise    = do { m <- ask; liftIO $ sock (c7 m) (s7 m) }
    where
        sock c (Just s) = do
            cval <- readIORef c
            case cval of
                (Just val) -> return $ Just val
                Nothing -> do
                    (newsock, _) <- accept s
                    handle <- socketToHandle newsock ReadWriteMode
                    writeIORef c $ Just handle
                    return $ Just handle
        sock _ _ = return Nothing


toStr = Prelude.map (chr . fromEnum) . unpack


-------------------------------------------------------------------------------
-- Protocol Constants
-- Protocol itself is simple,
-- suem send commands to devices, they response.
-- Example communication:
-- s: h ababe
-- d: o 07
-- s: W ababe 1999
-- d: o
-- d: i
-- s: l abab3
-- d: x
-- All numbers are in hex and have correct length
-- (zeroes on the left are mandatory).

-- requests
device_get_high_byte = "h" -- <5 hexes>; 2 hexes in response
device_get_low_byte  = "l" -- <5 hexes>; 2 hexes in response
device_get_word      = "w" -- <5 hexes>; 4 hexes in response

device_set_high_byte = "H" -- <5 hexes> <2 hexes>
device_set_low_byte  = "L" -- <5 hexes> <2 hexes>
device_set_word      = "W" -- <5 hexes> <4 hexes>

-- responses
device_interrupt = "i" -- and nothing else
device_ok        = "o" -- maybe number in response to get (with correct size)
device_bad       = "x" -- and nothing else =)


toHex :: Long -> Int -> String
toHex _ 0 = ""
toHex num digits = toHex (div num 16) (digits - 1)
                 ++ ["0123456789abcdef"
                    !! fromIntegral (num `mod` fromIntegral 16)]

fromHex :: String -> Int -> Long -> Long
fromHex _ 0 a = a
fromHex (d:ds) n a = fromHex ds (n - 1)
            (a * (fromIntegral 16) + maybe (error "") fromIntegral
                        (Data.List.elemIndex d "0123456789abcdef"))


readOk :: Handle -> Emulator ()
readOk h = do
    line <- liftIO $ Data.ByteString.hGetLine h
    if line == fromString device_ok
    then return ()
    else if line == fromString device_bad
         then error "Device sent BAD signal."
         else if line == fromString device_interrupt
              then do
                  m <- ask
                  interruptLevel <- getFnInterruptLevel m
                  if interruptLevel == 0
                  then do
                      getFnDoInterrupt m
                      readOk h
                  else readOk h
              else error "Unknown Device Protocol Line."

readOkWord :: Handle -> Emulator Word
readOkWord h = do
    line <- liftIO $ Data.ByteString.hGetLine h
    if fromString device_ok `Data.ByteString.isPrefixOf` line
    then let (_:ltail) = Prelude.drop (Prelude.length device_ok) (toStr line)
        in return $ fromIntegral $ fromHex ltail 4 (fromIntegral 0)
    else if line == fromString device_bad
         then error "Device sent BAD signal."
         else if line == fromString device_interrupt
              then do
                  m <- ask
                  interruptLevel <- getFnInterruptLevel m
                  if interruptLevel == 0
                  then do
                      getFnDoInterrupt m
                      readOkWord h
                  else readOkWord h
              else error "Unknown Device Protocol Line."

readOkByte :: Handle -> Emulator Byte
readOkByte h = do
    line <- liftIO $ Data.ByteString.hGetLine h
    if fromString device_ok `Data.ByteString.isPrefixOf` line
    then let (_:ltail) = Prelude.drop (Prelude.length device_ok) (toStr line)
        in return $ fromIntegral $ fromHex ltail 2 (fromIntegral 0)
    else if line == fromString device_bad
         then error "Device sent BAD signal."
         else if line == fromString device_interrupt
              then do
                  m <- ask
                  interruptLevel <- getFnInterruptLevel m
                  if interruptLevel == 0
                  then do
                      getFnDoInterrupt m
                      readOkByte h
                  else readOkByte h
              else error "Unknown Device Protocol Line."


-------------------------------------------------------------------------------
-- Memory

deviceGetByte :: Long -> Emulator Byte
deviceGetByte a
    | a `mod` 2 == 0 = do
        handle <- getSock a
        if handle == Nothing
        then return 0xff
        else do
            let (Just h) = handle
            liftIO $ hPut h $ fromString $
                device_get_high_byte ++ " " ++ toHex a 5 ++"\n"
            readOkByte h
    | otherwise = do
        handle <- getSock a
        if handle == Nothing
        then return 0xff
        else do
            let (Just h) = handle
            liftIO $ hPut h $ fromString $
                device_get_low_byte ++ " " ++ toHex a 5 ++"\n"
            readOkByte h

deviceGetWord :: Long -> Emulator Word
deviceGetWord a = do
    handle <- getSock a
    if handle == Nothing
    then return 0xffff
    else do
        let (Just h) = handle
        liftIO $ hPut h $ fromString $
            device_get_word ++ " " ++ toHex a 5 ++"\n"
        readOkWord h


deviceSetByte :: Long -> Byte -> Emulator ()
deviceSetByte a b
    | a `mod` 2 == 0 = do
        handle <- getSock a
        if handle == Nothing
        then return ()
        else do
            let (Just h) = handle
            liftIO $ hPut h $ fromString $
                device_set_high_byte ++ " " ++ toHex a 5 ++ " "
                                     ++ toHex (fromIntegral b) 2 ++"\n"
            readOk h
    | otherwise = do
        handle <- getSock a
        if handle == Nothing
        then return ()
        else do
            let (Just h) = handle
            liftIO $ hPut h $ fromString $
                device_set_low_byte ++ " " ++ toHex a 5 ++ " "
                                    ++ toHex (fromIntegral b) 2 ++"\n"
            readOk h

deviceSetWord :: Long -> Word -> Emulator ()
deviceSetWord a w = do
    handle <- getSock a
    if handle == Nothing
    then return ()
    else do
        let (Just h) = handle
        liftIO $ hPut h $ fromString $
            device_set_word ++ " " ++ toHex a 5 ++ " "
                            ++ toHex (fromIntegral w) 4 ++"\n"
        readOk h


-------------------------------------------------------------------------------
-- Interrupts

checkInteruptsFromDevices :: Emulator ()
checkInteruptsFromDevices = return ()
