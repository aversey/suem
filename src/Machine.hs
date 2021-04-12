{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module describes the basic types for our machine.
module Machine where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Prelude hiding (Word)
import Data.Word (Word32, Word16, Word8)
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.Trans (MonadIO)
import Network.Socket
import System.IO
import Utils


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
    s7  :: Maybe Socket,
    c0  :: IORef (Maybe Handle),
    c1  :: IORef (Maybe Handle),
    c2  :: IORef (Maybe Handle),
    c3  :: IORef (Maybe Handle),
    c4  :: IORef (Maybe Handle),
    c5  :: IORef (Maybe Handle),
    c6  :: IORef (Maybe Handle),
    c7  :: IORef (Maybe Handle),
    -- Deps for Devices
    getFnInterruptLevel :: Emulator Int,
    getFnDoInterrupt    :: Emulator ()
}

-- Emulator is a monad which contains Machine and allows easy change of it.
newtype Emulator a = Emulator (ReaderT Machine IO a)
    deriving (Monad, Applicative, Functor, MonadIO, MonadReader Machine)

with :: (Machine -> b) -> (b -> IO a) -> Emulator a
with field f = do
    m <- ask
    liftIO $ f (field m)
