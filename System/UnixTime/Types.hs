module System.UnixTime.Types where

import Data.ByteString
import Foreign.C.Types

type UnixTime = CTime

type Format = ByteString

