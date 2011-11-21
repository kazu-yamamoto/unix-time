{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnixTime.Diff (
    diffUnixTime
  , addUnixDiffTime
  , secondsToUnixDiffTime
  , microSecondsToUnixDiffTime
  ) where

import Data.UnixTime.Types
import Data.Int
import Foreign.C.Types

----------------------------------------------------------------

calc :: CTime -> Int32 -> UnixDiffTime
calc sec usec = uncurry UnixDiffTime . ajust sec $ usec

calc' :: CTime -> Int32 -> UnixDiffTime
calc' sec usec = uncurry UnixDiffTime . slowAjust sec $ usec

calcU :: CTime -> Int32 -> UnixTime
calcU sec usec = uncurry UnixTime . ajust sec $ usec

-- | Arithmetic operations where (1::UnixDiffTime) means 1 second.
instance Num UnixDiffTime where
	UnixDiffTime s1 u1 + UnixDiffTime s2 u2 = calc (s1+s2) (u1+u2)
	UnixDiffTime s1 u1 - UnixDiffTime s2 u2 = calc (s1-s2) (u1-u2)
	UnixDiffTime s1 u1 * UnixDiffTime s2 u2 = calc' (s1*s2) (u1*u2)
	negate (UnixDiffTime s u) = UnixDiffTime (-s) (-u)
	abs (UnixDiffTime s u) = UnixDiffTime (abs s) (abs u)
	signum (UnixDiffTime s u)
         | s == 0 && u == 0 = 0
         | s > 0            = 1
         | otherwise        = -1
	fromInteger i = UnixDiffTime (fromInteger i) 0

{-# RULES "Int->UnixDiffTime" fromIntegral = secondsToUnixDiffTime #-}

----------------------------------------------------------------

-- | Calculating difference between two 'UnixTime'.
diffUnixTime :: UnixTime -> UnixTime -> UnixDiffTime
diffUnixTime (UnixTime s1 u1) (UnixTime s2 u2) = calc (s1-s2) (u1-u2)

-- | Adding difference to 'UnixTime'.
addUnixDiffTime :: UnixTime -> UnixDiffTime -> UnixTime
addUnixDiffTime (UnixTime s1 u1) (UnixDiffTime s2 u2) = calcU (s1+s2) (u1+u2)

-- | Creating difference from seconds.
secondsToUnixDiffTime :: Int -> UnixDiffTime
secondsToUnixDiffTime sec = UnixDiffTime (fromIntegral sec) 0

-- | Creating difference from micro seconds.
microSecondsToUnixDiffTime :: Int -> UnixDiffTime
microSecondsToUnixDiffTime usec = calc (fromIntegral s) (fromIntegral u)
  where
    (s,u) = secondMicro usec

----------------------------------------------------------------

ajust :: CTime -> Int32 -> (CTime, Int32)
ajust sec usec
  | sec >= 0  = ajp
  | otherwise = ajm
  where
    micro  = 1000000
    mmicro = - micro
    ajp
     | usec >= micro  = (sec + 1, usec - micro)
     | usec >= 0      = (sec, usec)
     | otherwise      = (sec - 1, usec + micro)
    ajm
     | usec <= mmicro = (sec - 1, usec + micro)
     | usec < 0       = (sec, usec)
     | otherwise      = (sec + 1, usec - micro)

slowAjust :: CTime -> Int32 -> (CTime, Int32)
slowAjust sec usec = (sec + fromIntegral s, usec - u)
  where
    (s,u) = secondMicro u

secondMicro :: Integral a => a -> (a,a)
secondMicro usec = usec `quotRem` 1000000
