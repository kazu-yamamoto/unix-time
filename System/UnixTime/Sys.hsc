{-# LANGUAGE ForeignFunctionInterface #-}

module System.UnixTime.Sys (getUnixTime) where

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.UnixTime.Types

#include <time.h>
#include <sys/time.h>

type CTimeVal = ()
type CTimeZone = ()

foreign import ccall unsafe "gettimeofday"
    gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

getUnixTime :: IO UnixTime
getUnixTime =
  allocaBytes (#const sizeof(struct timeval)) $ \ p_timeval -> do
    throwErrnoIfMinus1_ "getClockTime" $ gettimeofday p_timeval nullPtr
    (#peek struct timeval,tv_sec) p_timeval
