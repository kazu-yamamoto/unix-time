{-# INCLUDE <time.h> #-}
{-# INCLUDE <sys/time.h> #-}
{-# LINE 1 "Sys.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "Sys.hsc" #-}

module Data.UnixTime.Sys (getUnixTime) where

import Data.UnixTime.Types
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- from System.Time


{-# LINE 15 "Sys.hsc" #-}

{-# LINE 16 "Sys.hsc" #-}

type CTimeVal = ()
type CTimeZone = ()

foreign import ccall unsafe "gettimeofday"
    gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

{-| Getting 'UnixTime' from OS.
-}
getUnixTime :: IO UnixTime
getUnixTime =
  allocaBytes (8) $ \ p_timeval -> do
{-# LINE 28 "Sys.hsc" #-}
    throwErrnoIfMinus1_ "getClockTime" $ gettimeofday p_timeval nullPtr
    sec <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_timeval
{-# LINE 30 "Sys.hsc" #-}
    usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p_timeval
{-# LINE 31 "Sys.hsc" #-}
    return $ UnixTime sec usec
