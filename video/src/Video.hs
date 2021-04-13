{-# LANGUAGE BinaryLiterals #-}
module Video (ConfigSocket(..), video) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Data.ByteString (pack, hPut, hGetNonBlocking)
import Data.Word
import Data.IORef
import Data.String
import Network.Socket
import System.IO
import Control.Monad


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


control :: Handle -> ViewPort -> Float -> [Word8] -> IO [Word8]
control h _ _ pic = do
    cmd <- hGetNonBlocking h 1
    return pic


video :: ConfigSocket -> IO ()
video sock = do
    s <- makeSocket sock
    h <- socketToHandle s ReadWriteMode
    let pic = replicate (256 * 256 * 4) 255
    simulateIO (InWindow "Suem Video" (256, 256) (0, 0)) black 60
               pic producePicture (control h)
