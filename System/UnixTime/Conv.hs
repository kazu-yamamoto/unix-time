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

formatUnixTime :: Format -> UnixTime -> IO ByteString
formatUnixTime fmt ut = 
    useAsCString fmt $ \cfmt -> do
        let siz = 256 -- FIXME
        ptr <- mallocBytes siz
        c_format_unix_time cfmt ut ptr (fromIntegral siz)
        unsafePackMallocCString ptr

parseUnixTime :: Format -> ByteString -> IO UnixTime
parseUnixTime fmt str =
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr ->
            c_parse_unix_time cfmt cstr


