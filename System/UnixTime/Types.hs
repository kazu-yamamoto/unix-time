module System.UnixTime.Types where

import Data.ByteString
import Data.Int
import Foreign.C.Types

data UnixTime = UnixTime {
    utSeconds :: CTime 
  , utMicroSeconds :: Int32
  } deriving (Eq,Ord,Show)

type Format = ByteString

