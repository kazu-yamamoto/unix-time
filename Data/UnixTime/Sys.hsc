{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnixTime.Sys (getUnixTime) where

import Data.UnixTime.Types
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- from System.Time

#include <time.h>
#include <sys/time.h>

type CTimeVal = ()
type CTimeZone = ()

foreign import ccall unsafe "gettimeofday"
    c_gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

-- |
-- Getting 'UnixTime' from OS.

getUnixTime :: IO UnixTime
getUnixTime =
  allocaBytes (#const sizeof(struct timeval)) $ \ p_timeval -> do
    throwErrnoIfMinus1_ "getClockTime" $ c_gettimeofday p_timeval nullPtr
    sec <- (#peek struct timeval,tv_sec) p_timeval
    usec <- (#peek struct timeval,tv_usec) p_timeval
    return $ UnixTime sec usec
