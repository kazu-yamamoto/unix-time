{-# LANGUAGE DeriveGeneric #-}

module Data.UnixTime.Types where

import Control.Applicative ((<$>), (<*>))
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
    peek ptr    = UnixTime
            <$> (#peek struct timeval, tv_sec)  ptr
            <*> (#peek struct timeval, tv_usec) ptr
    poke ptr ut = do
            (#poke struct timeval, tv_sec)  ptr (utSeconds ut)
            (#poke struct timeval, tv_usec) ptr (utMicroSeconds ut)
#endif

#if __GLASGOW_HASKELL__ >= 704
instance Binary UnixTime where
        put (UnixTime (CTime sec) msec) = do
            put sec
            put msec
        get = UnixTime <$> (CTime `fmap` get) <*> get
#endif

-- |
-- Format of the strptime()/strftime() style.
type Format = ByteString

-- |
-- Data structure for UnixTime diff.
--
-- It is up to the user to ensure that @'udtMicroSeconds' < 1000000@.
-- Helpers such as 'Data.UnixTime.microSecondsToUnixDiffTime' can help
-- you to create valid values. For example, it's a mistake to use
-- 'Data.Text.addUnixDiffTime' with a value @UnixDiffTime 0 9999999@
-- as it will produce an incorrect value back. You should instead use
-- functions such as 'Data.UnixTime.microSecondsToUnixDiffTime' to
-- create values that are in-range. This avoids any gotchas when then
-- doing comparisons.
data UnixDiffTime = UnixDiffTime {
    -- | Seconds
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
