{-# LANGUAGE ForeignFunctionInterface #-}

module System.UnixTime.Conv (formatUnixTime, parseUnixTime) where

import Data.ByteString
import Data.ByteString.Unsafe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.UnixTime.Types

foreign import ccall unsafe "c_parse_unix_time"
        c_parse_unix_time :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_format_unix_time"
        c_format_unix_time :: CString -> CTime -> CString -> CInt -> IO ()

{-| Formatting 'UnixTime' to 'ByteString'.
    This is a wrapper for strftime().
    This sets the locale to C but no guarantee.
-}
formatUnixTime :: Format -> UnixTime -> IO ByteString
formatUnixTime fmt (UnixTime sec _) =
    useAsCString fmt $ \cfmt -> do
        let siz = 256 -- FIXME
        ptr <- mallocBytes siz
        c_format_unix_time cfmt sec ptr (fromIntegral siz)
        unsafePackMallocCString ptr

{-| Parsing 'ByteString' to 'UnixTime'.
    This is a wrapper for strptime().
    This sets the locale to C but no guarantee.
-}
parseUnixTime :: Format -> ByteString -> IO UnixTime
parseUnixTime fmt str =
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- c_parse_unix_time cfmt cstr
            return $ UnixTime sec 0


