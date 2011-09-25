{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module System.UnixTime.Types where

import Data.ByteString
import Data.Int
import Foreign.C.Types
import Foreign.Storable

#if !(__GLASGOW_HASKELL__ >= 702)
newtype CSUSeconds = CSUSeconds Int64 deriving (Eq,Ord,Num,Enum,Storable,Real,Read)
instance Show CSUSeconds where
    show (CSUSeconds x) = show x
#endif

data UnixTime = UnixTime CTime CSUSeconds deriving (Eq,Ord,Show)

type Format = ByteString

