{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module describes the basic types and operations for our machine.
module Machine where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Data.Bits (testBit, setBit, clearBit)
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

readD :: Int -> Int -> Emulator Long
readD 0 s = with drs $ \rs -> do
    (r,_,_,_,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readD 1 s = with drs $ \rs -> do
    (_,r,_,_,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readD 2 s = with drs $ \rs -> do
    (_,_,r,_,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readD 3 s = with drs $ \rs -> do
    (_,_,_,r,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readD 4 s = with drs $ \rs -> do
    (_,_,_,_,r,_,_,_) <- readIORef rs
    return $ convertLong r s
readD 5 s = with drs $ \rs -> do
    (_,_,_,_,_,r,_,_) <- readIORef rs
    return $ convertLong r s
readD 6 s = with drs $ \rs -> do
    (_,_,_,_,_,_,r,_) <- readIORef rs
    return $ convertLong r s
readD 7 s = with drs $ \rs -> do
    (_,_,_,_,_,_,_,r) <- readIORef rs
    return $ convertLong r s
readD _ _ = return $ error "Incorrect Data register read"

readA :: Int -> Int -> Emulator Long
readA 0 s = with ars $ \rs -> do
    (r,_,_,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readA 1 s = with ars $ \rs -> do
    (_,r,_,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readA 2 s = with ars $ \rs -> do
    (_,_,r,_,_,_,_) <- readIORef rs
    return $ convertLong r s
readA 3 s = with ars $ \rs -> do
    (_,_,_,r,_,_,_) <- readIORef rs
    return $ convertLong r s
readA 4 s = with ars $ \rs -> do
    (_,_,_,_,r,_,_) <- readIORef rs
    return $ convertLong r s
readA 5 s = with ars $ \rs -> do
    (_,_,_,_,_,r,_) <- readIORef rs
    return $ convertLong r s
readA 6 s = with ars $ \rs -> do
    (_,_,_,_,_,_,r) <- readIORef rs
    return $ convertLong r s
readA 7 s = isSupervisor >>= \sup -> if sup
    then with ssp $ \sp -> do
        v <- readIORef sp
        return $ convertLong v s
    else with usp $ \sp -> do
        v <- readIORef sp
        return $ convertLong v s
readA _ _ = return $ error "Incorrect Address register read"


writeD :: Int -> Int -> Long -> Emulator ()
writeD 0 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (combineLong r r0 s,r1,r2,r3,r4,r5,r6,r7)
writeD 1 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,combineLong r r1 s,r2,r3,r4,r5,r6,r7)
writeD 2 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,combineLong r r2 s,r3,r4,r5,r6,r7)
writeD 3 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,combineLong r r3 s,r4,r5,r6,r7)
writeD 4 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,combineLong r r4 s,r5,r6,r7)
writeD 5 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r0,r2,r3,r4,combineLong r r5 s,r6,r7)
writeD 6 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,combineLong r r6 s,r7)
writeD 7 s r = with drs $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6,r7) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,r6,combineLong r r7 s)
writeD _ _ _ = return $ error "Incorrect Data register write"

writeA :: Int -> Int -> Long -> Emulator ()
writeA 0 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (combineLong r r0 s,r1,r2,r3,r4,r5,r6)
writeA 1 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,combineLong r r1 s,r2,r3,r4,r5,r6)
writeA 2 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,combineLong r r2 s,r3,r4,r5,r6)
writeA 3 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r2,combineLong r r3 s,r4,r5,r6)
writeA 4 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,combineLong r r4 s,r5,r6)
writeA 5 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r0,r2,r3,r4,combineLong r r5 s,r6)
writeA 6 s r = with ars $ \rs -> do
    (r0,r1,r2,r3,r4,r5,r6) <- readIORef rs
    writeIORef rs (r0,r1,r2,r3,r4,r5,combineLong r r6 s)
writeA 7 s r = isSupervisor >>= \sup -> if sup
    then with ssp $ \sp -> do
        v <- readIORef sp
        writeIORef sp $ combineLong r v s
    else with usp $ \sp -> do
        v <- readIORef sp
        writeIORef sp $ combineLong r v s
writeA _ _ _ = return $ error "Incorrect Address register write"


-------------------------------------------------------------------------------
-- PC Register Access

readPC = with pc $ \pc -> do
    pc <- readIORef pc
    return pc

writePC r = with pc $ \pc -> do
    writeIORef pc r

incPC = with pc $ \pc -> do
    pcval <- readIORef pc
    writeIORef pc (pcval + 2)


-------------------------------------------------------------------------------
-- Status Register Access

readSR = with sr $ \sr -> do
    sr <- readIORef sr
    return sr


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


setTracing :: Bool -> Emulator ()
setTracing b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 0

setSupervisor :: Bool -> Emulator ()
setSupervisor b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 2

-- setInterruptLevel :: Int -> Emulator ()

setExtend :: Bool -> Emulator ()
setExtend b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 11

setNegative :: Bool -> Emulator ()
setNegative b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 12

setZero :: Bool -> Emulator ()
setZero b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 13

setOverflow :: Bool -> Emulator ()
setOverflow b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 14

setCarry :: Bool -> Emulator ()
setCarry b = with sr $ \sr -> do
    srval <- readIORef sr
    writeIORef sr $ (if b then setBit else clearBit) srval 15


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


getMemory :: Long -> Int -> Emulator Long
getMemory a 1 = do
    val <- getByte a
    return $ fromIntegral val
getMemory a 2 = do
    val <- getWord a
    return $ fromIntegral val
getMemory a 4 = do
    val <- getLong a
    return $ fromIntegral val
getMemory _ _ = error "Bad size of getMemory"

setMemory :: Long -> Int -> Long -> Emulator ()
setMemory a 1 v = setByte a $ fromIntegral v
setMemory a 2 v = setWord a $ fromIntegral v
setMemory a 4 v = setLong a $ fromIntegral v
setMemory _ _ _ = error "Bad size of setMemory"


-------------------------------------------------------------------------------
-- Operand Access

skipOp :: Int -> Emulator ()
skipOp 1 = incPC
skipOp 2 = incPC
skipOp 4 = do
    incPC
    incPC
skipOp _ = error "Bad skipOp"

getOp :: Int -> Int -> Int
      -> Emulator (Emulator Long, Long -> Emulator ())
getOp 0 dr s = return (readD dr s, writeD dr s)
getOp 1 ar s = return (readA ar s, writeA ar s)
getOp 2 ar s = do
    addr <- readA ar 4
    return (getMemory addr s, setMemory addr s)
getOp 3 ar s = do
    addr <- readA ar 4
    writeA ar 4 (addr + (fromIntegral s))
    return (getMemory addr s, setMemory addr s)
getOp 4 ar s = do
    addr <- readA ar 4
    let addr = addr - (fromIntegral s)
    writeA ar 4 addr
    return (getMemory addr s, setMemory addr s)
getOp 5 ar s = do
    pc <- readPC
    incPC
    disp <- getMemory pc 2
    addr <- readA ar 4
    let addr = addr + disp
    return (getMemory addr s, setMemory addr s)
getOp 6 ar s = do
    pc <- readPC
    incPC
    prefix <- getMemory pc 1
    index <- (if testBit prefix 0 then readA else readD)
        (extractBits prefix [1..3])
        ((extractBits prefix [4] + 1) * 2)
    disp <- getMemory (pc + 1) 1
    addr <- readA ar 4
    let addr = addr + index + disp
    return (getMemory addr s, setMemory addr s)
getOp 7 2 s = do
    addr <- readPC
    incPC
    disp <- getMemory addr 2
    let addr = addr + disp
    return (getMemory addr s, setMemory addr s)
getOp 7 3 s = do
    addr <- readPC
    incPC
    prefix <- getMemory addr 1
    index <- (if testBit prefix 0 then readA else readD)
        (extractBits prefix [1..3])
        ((extractBits prefix [4] + 1) * 2)
    disp <- getMemory (addr + 1) 1
    let addr = addr + index + disp
    return (getMemory addr s, setMemory addr s)
getOp 7 0 s = do
    pc <- readPC
    incPC
    addr <- getMemory pc 2
    return (getMemory addr s, setMemory addr s)
getOp 7 1 s = do
    pc <- readPC
    skipOp 4
    addr <- getLong pc
    return (getMemory addr s, setMemory addr s)
getOp 7 6 s = do
    addr <- readPC
    skipOp s
    let addr = addr + if s == 1 then 1 else 0
    return (getMemory addr s, setMemory addr s)
