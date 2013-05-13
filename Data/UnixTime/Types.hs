module Data.UnixTime.Types where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int
import Foreign.C.Types

-- |
-- Data structure for Unix time.
data UnixTime = UnixTime {
    -- | Seconds from 1st Jan 1970
    utSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)

-- |
-- Format of the strptime()/strftime() style.
type Format = ByteString

-- |
-- Data structure for UnixTime diff.
--
-- >>> (3 :: UnixDiffTime) + 2
-- UnixDiffTime 5 0
-- >>> (2 :: UnixDiffTime) - 5
-- UnixDiffTime (-3) 0
-- >>> (3 :: UnixDiffTime) * 2
-- UnixDiffTime 6 0

data UnixDiffTime = UnixDiffTime {
    -- | Seconds from 1st Jan 1970
    udtSeconds :: {-# UNPACK #-} !CTime
    -- | Micro seconds (i.e. 10^(-6))
  , udtMicroSecnods :: {-# UNPACK #-} !Int32
  } deriving (Eq,Ord,Show)
