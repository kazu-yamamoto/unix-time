{-# LANGUAGE ForeignFunctionInterface #-}

module System.UnixTime.Conv (
    formatUnixTime, formatUnixTimeGMT, parseUnixTime
  , fromEpochTime, toEpochTime
  , fromClockTime, toClockTime
  ) where

import Data.ByteString
import Data.ByteString.Unsafe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (EpochTime)
import System.Time (ClockTime(..))
import System.UnixTime.Types

foreign import ccall unsafe "c_parse_unix_time"
        c_parse_unix_time :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_format_unix_time"
        c_format_unix_time :: CString -> CTime -> CString -> CInt -> IO ()

foreign import ccall unsafe "c_format_unix_time_gmt"
        c_format_unix_time_gmt :: CString -> CTime -> CString -> CInt -> IO ()

{-| Formatting 'UnixTime' to 'ByteString' in local time.
    This is a wrapper for strftime_l().
-}
formatUnixTime :: Format -> UnixTime -> ByteString
formatUnixTime fmt (UnixTime sec _) = unsafePerformIO $
    useAsCString fmt $ \cfmt -> do
        let siz = 256 -- FIXME
        ptr <- mallocBytes siz
        c_format_unix_time cfmt sec ptr (fromIntegral siz)
        unsafePackMallocCString ptr

{-| Formatting 'UnixTime' to 'ByteString' in GMT.
    This is a wrapper for strftime_l().
-}
formatUnixTimeGMT :: Format -> UnixTime -> ByteString
formatUnixTimeGMT fmt (UnixTime sec _) = unsafePerformIO $
    useAsCString fmt $ \cfmt -> do
        let siz = 256 -- FIXME
        ptr <- mallocBytes siz
        c_format_unix_time_gmt cfmt sec ptr (fromIntegral siz)
        unsafePackMallocCString ptr

{-| Parsing 'ByteString' to 'UnixTime'.
    This is a wrapper for strptime_l().
-}
parseUnixTime :: Format -> ByteString -> UnixTime
parseUnixTime fmt str = unsafePerformIO $
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- c_parse_unix_time cfmt cstr
            return $ UnixTime sec 0

----------------------------------------------------------------

{-| From 'EpochTime' to 'UnixTime' setting 'utMicroSeconds' to 0.
-}
fromEpochTime :: EpochTime -> UnixTime
fromEpochTime sec = UnixTime sec 0

{-| From 'UnixTime' to 'EpochTime' ignoring 'utMicroSeconds'.
-}
toEpochTime :: UnixTime -> EpochTime
toEpochTime (UnixTime sec _) = sec

{-| From 'ClockTime' to 'UnixTime'.
-}
fromClockTime :: ClockTime -> UnixTime
fromClockTime (TOD sec psec) = UnixTime sec' usec'
  where
    sec' = fromIntegral sec
    usec' = fromIntegral $ psec `div` 1000000

{-| From 'UnixTime' to 'ClockTime'.
-}
toClockTime :: UnixTime -> ClockTime
toClockTime (UnixTime sec usec) = TOD sec' psec'
  where
    sec' = truncate (toRational sec)
    psec' = fromIntegral $ usec * 1000000
