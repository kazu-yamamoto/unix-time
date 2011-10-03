{-# LANGUAGE OverloadedStrings #-}

module System.UnixTime.Types where

import Data.ByteString
import Data.ByteString.Char8 ()
import Data.Int
import Foreign.C.Types

{-| Data structure for Unix time.
-}
data UnixTime = UnixTime {
  -- | Seconds from 1st Jan 1970
    utSeconds :: CTime
  -- | Micro seconds (i.e. 10^(-6))
  , utMicroSeconds :: Int32
  } deriving (Eq,Ord,Show)

-- | Format of the strptime()/strftime() style.
type Format = ByteString

{-| Format for web (RFC 2616).
-}
webDateFormat :: Format
webDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

{-| Format for e-mail (RFC 5322).
-}
mailDateFormat :: Format
mailDateFormat = "%a, %d %b %Y %H:%M:%S %z"

