{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.UnixTime.Conv (
    formatUnixTime,
    formatUnixTimeGMT,
    parseUnixTime,
    parseUnixTimeGMT,
    webDateFormat,
    mailDateFormat,
    fromEpochTime,
    toEpochTime
) where

import Control.Applicative
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.ByteString.Char8
import Data.ByteString.Unsafe
import Data.UnixTime.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (EpochTime)

-- $setup
-- >>> import Data.Function (on)
-- >>> :set -XOverloadedStrings

foreign import ccall unsafe "c_parse_unix_time"
    c_parse_unix_time :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_parse_unix_time_gmt"
    c_parse_unix_time_gmt :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_format_unix_time"
    c_format_unix_time :: CString -> CTime -> CString -> CInt -> IO CSize

foreign import ccall unsafe "c_format_unix_time_gmt"
    c_format_unix_time_gmt :: CString -> CTime -> CString -> CInt -> IO CSize

-- The GMT functions temporarily change the process-global TZ.
-- Serialize all C calls so that concurrent calls cannot corrupt TZ.
tzLock :: MVar ()
tzLock = unsafePerformIO $ newMVar ()
{-# NOINLINE tzLock #-}

withTZLock :: IO a -> IO a
withTZLock = withMVar tzLock . const

----------------------------------------------------------------

-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as localtime.
-- This is a wrapper for strptime_l().
-- Many implementations of strptime_l() do not support %Z and
-- some implementations of strptime_l() do not support %z, either.
-- 'utMicroSeconds' is always set to 0.
parseUnixTime :: Format -> ByteString -> UnixTime
parseUnixTime fmt str = unsafePerformIO $
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- withTZLock $ c_parse_unix_time cfmt cstr
            return $ UnixTime sec 0

-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as GMT.
-- This is a wrapper for strptime_l().
-- 'utMicroSeconds' is always set to 0.
--
-- >>> parseUnixTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
-- UnixTime {utSeconds = 0, utMicroSeconds = 0}
parseUnixTimeGMT :: Format -> ByteString -> UnixTime
parseUnixTimeGMT fmt str = unsafePerformIO $
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- withTZLock $ c_parse_unix_time_gmt cfmt cstr
            return $ UnixTime sec 0

----------------------------------------------------------------

-- |
-- Formatting 'UnixTime' to 'ByteString' in local time.
-- This is a wrapper for strftime_l().
-- 'utMicroSeconds' is ignored.
-- The result depends on the TZ environment variable.
formatUnixTime :: Format -> UnixTime -> IO ByteString
formatUnixTime fmt t =
    formatUnixTimeHelper c_format_unix_time fmt t
{-# INLINE formatUnixTime #-}

-- |
-- Formatting 'UnixTime' to 'ByteString' in GMT.
-- This is a wrapper for strftime_l().
-- 'utMicroSeconds' is ignored.
--
-- >>> formatUnixTimeGMT webDateFormat $ UnixTime 0 0
-- "Thu, 01 Jan 1970 00:00:00 GMT"
-- >>> let ut = UnixTime 100 200
-- >>> let str = formatUnixTimeGMT "%s" ut
-- >>> let ut' = parseUnixTimeGMT "%s" str
-- >>> ((==) `on` utSeconds) ut ut'
-- True
-- >>> ((==) `on` utMicroSeconds) ut ut'
-- False
formatUnixTimeGMT :: Format -> UnixTime -> ByteString
formatUnixTimeGMT fmt t =
    unsafePerformIO $ formatUnixTimeHelper c_format_unix_time_gmt fmt t
{-# INLINE formatUnixTimeGMT #-}

-- |
-- Helper handling memory allocation for formatUnixTime and formatUnixTimeGMT.
formatUnixTimeHelper
    :: (CString -> CTime -> CString -> CInt -> IO CSize)
    -> Format
    -> UnixTime
    -> IO ByteString
formatUnixTimeHelper formatFun fmt (UnixTime sec _) =
    useAsCString fmt $ \cfmt -> loop cfmt 80
  where
    -- strftime() returns 0 if the result does not fit in the buffer.
    -- Retry with a larger buffer but give up at a bound.
    limit = 80 + 64 * Data.ByteString.Char8.length fmt
    loop cfmt siz = do
        ptr <- mallocBytes siz
        len <- withTZLock $ fromIntegral <$> formatFun cfmt sec ptr (fromIntegral siz)
        if len == 0 && siz < limit
            then free ptr >> loop cfmt (siz * 2)
            else do
                ptr' <- reallocBytes ptr (max 1 len)
                unsafePackMallocCStringLen (ptr', len)

----------------------------------------------------------------

-- |
-- Format for web (RFC 2616).
-- The value is \"%a, %d %b %Y %H:%M:%S GMT\".
-- This should be used with 'formatUnixTimeGMT' and 'parseUnixTimeGMT'.
webDateFormat :: Format
webDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

-- |
-- Format for e-mail (RFC 5322).
-- The value is \"%a, %d %b %Y %H:%M:%S %z\".
-- This should be used with 'formatUnixTime' and 'parseUnixTime'.
mailDateFormat :: Format
mailDateFormat = "%a, %d %b %Y %H:%M:%S %z"

----------------------------------------------------------------

-- |
-- From 'EpochTime' to 'UnixTime' setting 'utMicroSeconds' to 0.
fromEpochTime :: EpochTime -> UnixTime
fromEpochTime sec = UnixTime sec 0

-- |
-- From 'UnixTime' to 'EpochTime' ignoring 'utMicroSeconds'.
toEpochTime :: UnixTime -> EpochTime
toEpochTime (UnixTime sec _) = sec
