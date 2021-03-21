{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module describes the basic types and operations for our machine.
module Machine where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Data.Bits (testBit)
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.Trans (MonadIO)
import Network.Socket
import Utils


-------------------------------------------------------------------------------
-- Base Types

type Long = Word32
type Word = Word16
type Byte = Word8

data Machine = Machine {
    pc  :: IORef Long,
    sr  :: IORef Word,
    drs :: IORef (Long, Long, Long, Long, Long, Long, Long, Long),
    ars :: IORef (Long, Long, Long, Long, Long, Long, Long),
    usp :: IORef Long,   -- this is a7 in user mode
    ssp :: IORef Long,   -- this is a7 in supermode
    ram :: VM.IOVector Byte,
    rom :: V.Vector Byte,
    s0  :: Maybe Socket,
    s1  :: Maybe Socket,
    s2  :: Maybe Socket,
    s3  :: Maybe Socket,
    s4  :: Maybe Socket,
    s5  :: Maybe Socket,
    s6  :: Maybe Socket,
    s7  :: Maybe Socket
}

-- Emulator is a monad which contains Machine and allows easy change of it.
newtype Emulator a = Emulator (ReaderT Machine IO a)
    deriving (Monad, Applicative, Functor, MonadIO, MonadReader Machine)

with :: (Machine -> b) -> (b -> IO a) -> Emulator a
with field f = do
    m <- ask
    liftIO $ f (field m)


-------------------------------------------------------------------------------
-- Data and Address Registers Access

readD :: Int -> Emulator Long
readD 0 = with drs $ \rs -> do
    (r,_,_,_,_,_,_,_) <- readIORef rs
    return r
readD 1 = with drs $ \rs -> do
    (_,r,_,_,_,_,_,_) <- readIORef rs
    return r
readD 2 = with drs $ \rs -> do
    (_,_,r,_,_,_,_,_) <- readIORef rs
    return r
readD 3 = with drs $ \rs -> do
    (_,_,_,r,_,_,_,_) <- readIORef rs
    return r
readD 4 = with drs $ \rs -> do
    (_,_,_,_,r,_,_,_) <- readIORef rs
    return r
readD 5 = with drs $ \rs -> do
    (_,_,_,_,_,r,_,_) <- readIORef rs
    return r
readD 6 = with drs $ \rs -> do
    (_,_,_,_,_,_,r,_) <- readIORef rs
    return r
readD 7 = with drs $ \rs -> do
    (_,_,_,_,_,_,_,r) <- readIORef rs
    return r
readD _ = return $ error "Incorrect Data register read"

readA :: Int -> Emulator Long
readA 0 = with ars $ \rs -> do
    (r,_,_,_,_,_,_) <- readIORef rs
    return r
readA 1 = with ars $ \rs -> do
    (_,r,_,_,_,_,_) <- readIORef rs
    return r
readA 2 = with ars $ \rs -> do
    (_,_,r,_,_,_,_) <- readIORef rs
    return r
readA 3 = with ars $ \rs -> do
    (_,_,_,r,_,_,_) <- readIORef rs
    return r
readA 4 = with ars $ \rs -> do
    (_,_,_,_,r,_,_) <- readIORef rs
    return r
readA 5 = with ars $ \rs -> do
    (_,_,_,_,_,r,_) <- readIORef rs
    return r
readA 6 = with ars $ \rs -> do
    (_,_,_,_,_,_,r) <- readIORef rs
    return r
readA 7 = isSupervisor >>= \sup -> if sup
    then with ssp $ \sp -> readIORef sp
    else with usp $ \sp -> readIORef sp
readA _ = return $ error "Incorrect Address register read"


writeD :: Int -> Long -> Emulator ()
writeD 0 r = with drs $ \rs -> do
    (_,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r,r1,r2,r3,r4,r5,r6,r7)
writeD 1 r = with drs $ \rs -> do
    (r0,_,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r,r2,r3,r4,r5,r6,r7)
writeD 2 r = with drs $ \rs -> do
    (r0,r1,_,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r,r3,r4,r5,r6,r7)
writeD 3 r = with drs $ \rs -> do
    (r0,r1,r2,_,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r,r4,r5,r6,r7)
writeD 4 r = with drs $ \rs -> do
    (r0,r1,r2,r3,_,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r,r5,r6,r7)
writeD 5 r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,_,r6,r7) <- readIORef rs
    writeIORef rs (r0,r0,r2,r3,r4,r,r6,r7)
writeD 6 r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,_,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,r,r7)
writeD 7 r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,_) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,r6,r)
writeD _ _ = return $ error "Incorrect Data register write"

writeA :: Int -> Long -> Emulator ()
writeA 0 r = with ars $ \rs -> do
    (_,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r,r1,r2,r3,r4,r5,r6)
writeA 1 r = with ars $ \rs -> do
    (r0,_,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r,r2,r3,r4,r5,r6)
writeA 2 r = with ars $ \rs -> do
    (r0,r1,_,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r,r3,r4,r5,r6)
writeA 3 r = with ars $ \rs -> do
    (r0,r1,r2,_,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r2,r,r4,r5,r6)
writeA 4 r = with ars $ \rs -> do
    (r0,r1,r2,r3,_,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r,r5,r6)
writeA 5 r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,_,r6) <- readIORef rs
    writeIORef rs (r0,r0,r2,r3,r4,r,r6)
writeA 6 r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,_) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,r)
writeA 7 r = isSupervisor >>= \sup -> if sup
    then with ssp $ \sp -> writeIORef sp r
    else with usp $ \sp -> writeIORef sp r
writeA _ _ = return $ error "Incorrect Address register write"


-------------------------------------------------------------------------------
-- Status Register Access

isTracing :: Emulator Bool
isTracing = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 0

isSupervisor :: Emulator Bool
isSupervisor = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 2

interruptLevel :: Emulator Int
interruptLevel = with sr $ \sr -> do
    sr <- readIORef sr
    return $ extractBits sr [5, 6, 7]

isExtend :: Emulator Bool
isExtend = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 11

isNegative :: Emulator Bool
isNegative = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 12

isZero :: Emulator Bool
isZero = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 13

isOverflow :: Emulator Bool
isOverflow = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 14

isCarry :: Emulator Bool
isCarry = with sr $ \sr -> do
    sr <- readIORef sr
    return $ testBit sr 15


-------------------------------------------------------------------------------
-- Memmory Access

getByte :: Long -> Emulator Byte
getByte a | a < 0x8 = with rom $ \rom -> return $ rom V.! fromIntegral a
          | a < 0x7e0000 = with ram $ \ram ->
              if VM.length ram >= fromIntegral a
              then VM.unsafeRead ram (fromIntegral a)
              else return 0xff
          | a < 0x800000 = with rom $ \rom ->
              return $ rom V.! (fromIntegral a - 0x7e0000)
          | otherwise = return 0xff

 -- TODO: only even addresses are allowed
getWord :: Long -> Emulator Word
getWord a = do
    g <- getByte a
    l <- getByte (a + 1)
    return $ (fromIntegral g) * 256 + (fromIntegral l)

 -- TODO: only even addresses are allowed
getLong :: Long -> Emulator Long
getLong a = do
    g <- getWord a
    l <- getWord (a + 2)
    return $ (fromIntegral g) * 256 * 256 + (fromIntegral l)


setByte :: Long -> Byte -> Emulator ()
setByte a b | a < 0x8 = return ()
            | a < 0x7e0000 = with ram $ \ram ->
                VM.write ram (fromIntegral a) b
            | otherwise = return ()

 -- TODO: only even addresses are allowed
setWord :: Long -> Word -> Emulator ()
setWord a w = do
    setByte a       (fromIntegral (rem (fromIntegral w) 256))
    setByte (a + 1) (fromIntegral (div (fromIntegral w) 256))

 -- TODO: only even addresses are allowed
setLong :: Long -> Long -> Emulator ()
setLong a l = do
    setWord a       (fromIntegral (rem (fromIntegral l) 256 * 256))
    setWord (a + 2) (fromIntegral (div (fromIntegral l) 256 * 256))
