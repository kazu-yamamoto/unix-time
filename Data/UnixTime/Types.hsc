{-# LANGUAGE DeriveGeneric #-}

module Data.UnixTime.Types where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int
#if defined(_WIN32)
import Data.Word
#endif
import Foreign.C.Types
import Foreign.Storable
#if __GLASGOW_HASKELL__ >= 704
import Data.Binary
#endif
import GHC.Generics

#include <sys/time.h>

-- |
-- Data structure for Unix time.
--
-- Please note that this uses GHC-derived 'Eq' and 'Ord' instances.
-- Notably
--
-- >>> UnixTime 1 0 > UnixTime 0 999999999
-- True
--
-- You should instead use 'UnixDiffTime' along with its helpers such
-- as 'Data.UnixTime.microSecondsToUnixDiffTime' which will ensure
-- that such unusual values are never created.
data UnixTime = UnixTime {
    -- | Seconds from 1st Jan 1970
    utSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show,Generic)

instance Storable UnixTime where
    sizeOf _    = (#size struct timeval)
    alignment _ = (#const offsetof(struct {char x__; struct timeval (y__); }, y__))
#if defined(_WIN32)
    -- On windows, with mingw-w64, the struct `timeval` is defined as
    --
    --      struct timeval
    --      {
    --          long tv_sec;
    --          long tv_usec;
    --      };
    --
    -- The type `long` is 32bit on windows 64bit, however the CTime is 64bit, thus
    -- we must be careful about the layout of its foreign memory.
    --
    -- Here we try use Word32, rather than Int32, to support as large value as possible.
    -- So, times before 1970 cannot be represented.
    -- For example, we use `4294967295` as utSeconds in testsuite, which already exceeds
    -- the range of Int32, but not for Word32.
    peek ptr    = do
            sec <- (#peek struct timeval, tv_sec) ptr :: IO Word32
            msec <- (#peek struct timeval, tv_usec) ptr
            return $ UnixTime (fromIntegral sec) msec
    poke ptr ut = do
            let CTime sec = utSeconds ut
            (#poke struct timeval, tv_sec)  ptr (fromIntegral sec :: Word32)
            (#poke struct timeval, tv_usec) ptr (utMicroSeconds ut)
#else
    -- On Unix, the struct `timeval` is defined as
    --
    --      struct timeval
    --      {
    --          time_t      tv_sec;
    --          suseconds_t tv_usec;
    --      };
    --
    -- The type `suseconds_t` is a signed integer type capable of storing
    -- values at least in the range `[-1, 1000000]`. It's size is platform
    -- specific, and it is 8 bytes long on 64-bit platforms.
    --
    -- Here we peek `tv_usec` using the `CSUSeconds` type and then convert it
    -- to `Int32` (the type of `utMicroSeconds`) relying on the fact that
    -- `tv_usec` is no bigger than `1000000`, and hence we will not overflow.
    peek ptr    = do
            sec <- (#peek struct timeval, tv_sec) ptr
            CSUSeconds msec <- (#peek struct timeval, tv_usec) ptr
            return $ UnixTime sec (fromIntegral msec)
    poke ptr ut = do
            let msec = CSUSeconds $ fromIntegral (utMicroSeconds ut)
            (#poke struct timeval, tv_sec)  ptr (utSeconds ut)
            (#poke struct timeval, tv_usec) ptr msec
#endif

#if __GLASGOW_HASKELL__ >= 704
instance Binary UnixTime where
        -- Seconds are always encoded as Int64 to be portable.
        put (UnixTime (CTime sec) msec) = do
            put (fromIntegral sec :: Int64)
            put msec
        get = do
            sec <- get :: Get Int64
            UnixTime (CTime (fromIntegral sec)) <$> get
#endif

-- |
-- Format of the strptime()/strftime() style.
type Format = ByteString

-- |
-- Data structure for UnixTime diff.
--
-- The functions in this library normalize values so that both fields
-- have the same sign and @|'udtMicroSeconds'| < 1000000@.
-- Use 'Data.UnixTime.microSecondsToUnixDiffTime' to create values.
-- The derived 'Eq' and 'Ord' instances are meaningful only for
-- normalized values.
data UnixDiffTime = UnixDiffTime {
    -- | Seconds
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
