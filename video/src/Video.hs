{-# LANGUAGE BinaryLiterals #-}
module Video (ConfigSocket(..), video) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Data.ByteString (ByteString, unpack, pack, hPut, hGetLine)
import Data.List (elemIndex, isPrefixOf)
import Data.Word
import Data.Char (chr)
import Data.Maybe
import Data.IORef
import Data.String
import Network.Socket
import System.IO
import Control.Monad


device_get_high_byte = "h"
device_get_low_byte  = "l"
device_get_word      = "w"

device_set_high_byte = "H"
device_set_low_byte  = "L"
device_set_word      = "W"

device_ok = "o"


data ConfigSocket = ConfigInet PortNumber | ConfigUnix String


makeSocket :: ConfigSocket -> IO Socket
makeSocket (ConfigUnix a) = do
    sock <- socket AF_UNIX Stream defaultProtocol
    Network.Socket.connect sock $ SockAddrUnix a
    return sock
makeSocket (ConfigInet p) = do
    sock <- socket AF_INET Stream defaultProtocol
    Network.Socket.connect sock $ SockAddrInet p 0x0100007f
    return sock


producePicture :: [Word8] -> IO Picture
producePicture pic = do
    return $ bitmapOfByteString 256 256 (BitmapFormat TopToBottom PxRGBA)
                                (pack pic) False


bsToStr :: ByteString -> String
bsToStr = map (chr . fromEnum) . unpack


toHex :: Word8 -> String
toHex n = ["0123456789abcdef" !! (fromIntegral n `div` 16 `mod` 16)] ++
          ["0123456789abcdef" !! (fromIntegral n `mod` 16)]

fromHex :: String -> Int -> Int -> Int -> Int
fromHex _ _ 0 a = a
fromHex s i n a = fromHex s (i + 1) (n - 1)
    (a * 16 + fromJust (Data.List.elemIndex (s !! i) "0123456789abcdef"))


replace :: Int -> Int -> [Word8] -> [Word8]
replace 0 v (_:t) = fromIntegral v : t
replace n v (h:t) = h : replace (n - 1) v t


doCommand :: Handle -> [Word8] -> IO [Word8]
doCommand h pic = do
    cmdbs <- Data.ByteString.hGetLine h
    let cmd = bsToStr cmdbs
    if device_get_high_byte `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0 in do
        hPut h $ fromString $ device_ok ++ " " ++ toHex (pic !! i) ++ "\n"
        return pic
    else if device_get_low_byte `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0 in do
        hPut h $ fromString $ device_ok ++ " " ++ toHex (pic!!(i+1)) ++ "\n"
        return pic
    else if device_get_word `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0 in do
        hPut h $ fromString $ device_ok ++ " "
            ++ toHex (pic !! i) ++ toHex (pic!!(i+1)) ++ "\n"
        return pic
    else if device_set_high_byte `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0
             v = fromHex cmd 8 2 0 in do
        hPut h $ fromString $ device_ok ++ "\n"
        return $ replace i v pic
    else if device_set_low_byte `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0
             v = fromHex cmd 8 2 0 in do
        hPut h $ fromString $ device_ok ++ "\n"
        return $ replace (i + 1) v pic
    else if device_set_word `isPrefixOf` cmd
    then let i = fromHex cmd 2 5 0
             vh = fromHex cmd 8 2 0
             vl = fromHex cmd 10 2 0 in do
        hPut h $ fromString $ device_ok ++ "\n"
        return $ replace i vh $ replace (i + 1) vl pic
    else error "Unknown Device Protocol Line"

control :: Handle -> ViewPort -> Float -> [Word8] -> IO [Word8]
control h _ _ pic = do
    cmd_ready <- hReady h
    if cmd_ready then doCommand h pic else return pic


video :: ConfigSocket -> IO ()
video sock = do
    s <- makeSocket sock
    h <- socketToHandle s ReadWriteMode
    let pic = replicate (256 * 256 * 4) 255
    simulateIO (InWindow "Suem Video" (256, 256) (0, 0)) black 60
               pic producePicture (control h)
