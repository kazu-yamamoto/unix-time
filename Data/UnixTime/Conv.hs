{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.UnixTime.Conv (
    formatUnixTime,
    formatUnixTimeGMT,
    parseUnixTime,
    parseUnixTimeGMT,
    parseUnixTimeMaybe,
    parseUnixTimeGMTMaybe,
    webDateFormat,
    mailDateFormat,
    fromEpochTime,
    toEpochTime
) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (createAndTrim)
import Data.Char (isDigit)
import Data.UnixTime.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (EpochTime)

-- $setup
-- >>> import Data.Function (on)
-- >>> :set -XOverloadedStrings

foreign import ccall unsafe "c_init_locale"
    c_init_locale :: IO ()

foreign import ccall unsafe "c_parse_unix_time"
    c_parse_unix_time :: CString -> CString -> Ptr CInt -> IO CTime

foreign import ccall unsafe "c_parse_unix_time_gmt"
    c_parse_unix_time_gmt :: CString -> CString -> Ptr CInt -> IO CTime

foreign import ccall unsafe "c_format_unix_time"
    c_format_unix_time :: CString -> CTime -> CString -> CInt -> IO CSize

foreign import ccall unsafe "c_format_unix_time_gmt"
    c_format_unix_time_gmt :: CString -> CTime -> CString -> CInt -> IO CSize

-- The C side must initialize its locale exactly once before use.
localeReady :: ()
localeReady = unsafePerformIO c_init_locale
{-# NOINLINE localeReady #-}

----------------------------------------------------------------

type CParser = CString -> CString -> Ptr CInt -> IO CTime

parseHelper :: CParser -> Format -> ByteString -> (UnixTime, Bool)
parseHelper parser fmt str = localeReady `seq` unsafePerformIO (
    BS.useAsCString fmt $ \cfmt ->
        BS.useAsCString str $ \cstr ->
            alloca $ \pok -> do
                sec <- parser cfmt cstr pok
                ok <- peek pok
                -- A NUL byte would silently truncate the C strings.
                let complete = BS.notElem '\0' fmt && BS.notElem '\0' str
                return (UnixTime sec 0, ok /= 0 && complete))

toMaybe :: (UnixTime, Bool) -> Maybe UnixTime
toMaybe (ut, True) = Just ut
toMaybe _ = Nothing

-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as localtime.
-- This is a wrapper for strptime_l().
-- Many implementations of strptime_l() do not support %Z and
-- some implementations of strptime_l() do not support %z, either.
-- 'utMicroSeconds' is always set to 0.
--
-- The result is unspecified if parsing fails.
-- Use 'parseUnixTimeMaybe' to detect failures.
--
-- This depends on the TZ environment variable but is a pure function.
-- So, the result may be shared even if TZ is changed later.
parseUnixTime :: Format -> ByteString -> UnixTime
parseUnixTime fmt str = fst $ parseHelper c_parse_unix_time fmt str

-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as GMT.
-- This is a wrapper for strptime_l().
-- 'utMicroSeconds' is always set to 0.
--
-- The result is unspecified if parsing fails.
-- Use 'parseUnixTimeGMTMaybe' to detect failures.
--
-- >>> parseUnixTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
-- UnixTime {utSeconds = 0, utMicroSeconds = 0}
parseUnixTimeGMT :: Format -> ByteString -> UnixTime
parseUnixTimeGMT fmt str = fst $ parseHelper c_parse_unix_time_gmt fmt str

-- |
-- Like 'parseUnixTime' but returns 'Nothing' unless the entire input
-- matches the format.
parseUnixTimeMaybe :: Format -> ByteString -> Maybe UnixTime
parseUnixTimeMaybe fmt str = toMaybe $ parseHelper c_parse_unix_time fmt str

-- |
-- Like 'parseUnixTimeGMT' but returns 'Nothing' unless the entire input
-- matches the format.
--
-- >>> parseUnixTimeGMTMaybe webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
-- Just (UnixTime {utSeconds = 0, utMicroSeconds = 0})
-- >>> parseUnixTimeGMTMaybe webDateFormat "garbage"
-- Nothing
parseUnixTimeGMTMaybe :: Format -> ByteString -> Maybe UnixTime
parseUnixTimeGMTMaybe fmt str = toMaybe $ parseHelper c_parse_unix_time_gmt fmt str

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
-- A format containing a NUL byte is truncated there.
formatUnixTimeHelper
    :: (CString -> CTime -> CString -> CInt -> IO CSize)
    -> Format
    -> UnixTime
    -> IO ByteString
formatUnixTimeHelper formatFun fmt (UnixTime sec _) =
    localeReady `seq` BS.useAsCString fmt (\cfmt -> loop cfmt 80)
  where
    -- strftime() returns 0 if the result does not fit in the buffer.
    -- Retry with a larger buffer but give up at a bound
    -- which also covers field widths such as "%100Y".
    limit :: Int
    limit = fromInteger $ min (1024 * 1024) $
        80 + 64 * toInteger (BS.length fmt) + sum (map (read . BS.unpack) widths)
    widths = filter (not . BS.null) $ BS.splitWith (not . isDigit) fmt
    loop cfmt siz = do
        bs <- createAndTrim siz $ \ptr ->
            fromIntegral <$> formatFun cfmt sec (castPtr ptr) (fromIntegral siz)
        if BS.null bs && siz < limit
            then loop cfmt (siz * 2)
            else return bs

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
