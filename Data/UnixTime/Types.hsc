module Data.UnixTime.Types where

import Control.Applicative ((<$>), (<*>))
import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int
import Foreign.C.Types
import Foreign.Storable
#if __GLASGOW_HASKELL__ >= 704
import Data.Binary
#endif

#include <sys/time.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- |
-- Data structure for Unix time.
data UnixTime = UnixTime {
    -- | Seconds from 1st Jan 1970
    utSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)

instance Storable UnixTime where
    sizeOf _    = (#size struct timeval)
    alignment _ = (#alignment struct timeval)
    peek ptr    = UnixTime
            <$> (#peek struct timeval, tv_sec)  ptr
            <*> (#peek struct timeval, tv_usec) ptr
    poke ptr ut = do
            (#poke struct timeval, tv_sec)  ptr (utSeconds ut)
            (#poke struct timeval, tv_usec) ptr (utMicroSeconds ut)

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
-- >>> (3 :: UnixDiffTime) + 2
-- UnixDiffTime {udtSeconds = 5, udtMicroSecnods = 0}
-- >>> (2 :: UnixDiffTime) - 5
-- UnixDiffTime {udtSeconds = -3, udtMicroSecnods = 0}
-- >>> (3 :: UnixDiffTime) * 2
-- UnixDiffTime {udtSeconds = 6, udtMicroSecnods = 0}

data UnixDiffTime = UnixDiffTime {
    -- | Seconds from 1st Jan 1970
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSecnods :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
