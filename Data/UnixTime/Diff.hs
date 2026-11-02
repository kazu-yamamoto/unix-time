{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnixTime.Diff (
    diffUnixTime,
    addUnixDiffTime,
    secondsToUnixDiffTime,
    microSecondsToUnixDiffTime,
) where

import Data.Int
import Data.UnixTime.Types
import Foreign.C.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

-- 'UnixDiffTime' is normalized so that both fields share the sign of
-- the value and @|udtMicroSeconds| < 1000000@.
normDiff :: CTime -> Int64 -> UnixDiffTime
normDiff sec usec
    | s > 0 && u < 0 = UnixDiffTime (s - 1) (fromIntegral (u + micro))
    | s < 0 && u > 0 = UnixDiffTime (s + 1) (fromIntegral (u - micro))
    | otherwise = UnixDiffTime s (fromIntegral u)
  where
    (q, u) = usec `quotRem` micro
    s = sec + fromIntegral q

-- 'UnixTime' is normalized so that @0 <= utMicroSeconds < 1000000@
-- as @struct timeval@.
normTime :: CTime -> Int64 -> UnixTime
normTime sec usec = UnixTime (sec + fromIntegral q) (fromIntegral u)
  where
    (q, u) = usec `divMod` micro

normalize :: UnixDiffTime -> UnixDiffTime
normalize (UnixDiffTime s u) = normDiff s (fromIntegral u)

toMicro :: UnixDiffTime -> Integer
toMicro (UnixDiffTime (CTime s) u) = toInteger s * micro + toInteger u

fromMicro :: Integer -> UnixDiffTime
fromMicro n = UnixDiffTime (fromInteger s) (fromInteger u)
  where
    (s, u) = n `quotRem` micro

micro :: Num a => a
micro = 1000000

-- | Arithmetic operations where (1::UnixDiffTime) means 1 second.
--
-- >>> (3 :: UnixDiffTime) + 2
-- UnixDiffTime {udtSeconds = 5, udtMicroSeconds = 0}
-- >>> (2 :: UnixDiffTime) - 5
-- UnixDiffTime {udtSeconds = -3, udtMicroSeconds = 0}
-- >>> (3 :: UnixDiffTime) * 2
-- UnixDiffTime {udtSeconds = 6, udtMicroSeconds = 0}
-- >>> microSecondsToUnixDiffTime (1500000 :: Int) * microSecondsToUnixDiffTime (1500000 :: Int)
-- UnixDiffTime {udtSeconds = 2, udtMicroSeconds = 250000}
-- >>> negate (microSecondsToUnixDiffTime (500000 :: Int))
-- UnixDiffTime {udtSeconds = 0, udtMicroSeconds = -500000}
instance Num UnixDiffTime where
    UnixDiffTime s1 u1 + UnixDiffTime s2 u2 =
        normDiff (s1 + s2) (fromIntegral u1 + fromIntegral u2)
    UnixDiffTime s1 u1 - UnixDiffTime s2 u2 =
        normDiff (s1 - s2) (fromIntegral u1 - fromIntegral u2)
    x * y = fromMicro (toMicro x * toMicro y `quot` micro)
    negate (UnixDiffTime s u) = normDiff (negate s) (negate (fromIntegral u))
    abs x = UnixDiffTime (abs s) (abs u)
      where
        UnixDiffTime s u = normalize x
    signum x
        | s > 0 || u > 0 = 1
        | s < 0 || u < 0 = -1
        | otherwise = 0
      where
        UnixDiffTime s u = normalize x
    fromInteger i = UnixDiffTime (fromInteger i) 0

{-# RULES "Integral->UnixDiffTime" fromIntegral = secondsToUnixDiffTime #-}

instance Real UnixDiffTime where
    toRational = toFractional

{-# RULES "UnixDiffTime->Fractional" realToFrac = toFractional #-}

----------------------------------------------------------------

-- | Calculating difference between two 'UnixTime'.
--
-- >>> UnixTime 100 2000 `diffUnixTime` UnixTime 98 2100
-- UnixDiffTime {udtSeconds = 1, udtMicroSeconds = 999900}
diffUnixTime :: UnixTime -> UnixTime -> UnixDiffTime
diffUnixTime (UnixTime s1 u1) (UnixTime s2 u2) =
    normDiff (s1 - s2) (fromIntegral u1 - fromIntegral u2)

-- | Adding difference to 'UnixTime'.
--
-- >>> UnixTime 100 2000 `addUnixDiffTime` microSecondsToUnixDiffTime ((-1003000) :: Int)
-- UnixTime {utSeconds = 98, utMicroSeconds = 999000}
addUnixDiffTime :: UnixTime -> UnixDiffTime -> UnixTime
addUnixDiffTime (UnixTime s1 u1) (UnixDiffTime s2 u2) =
    normTime (s1 + s2) (fromIntegral u1 + fromIntegral u2)

-- | Creating difference from seconds.
--
-- >>> secondsToUnixDiffTime (100 :: Int)
-- UnixDiffTime {udtSeconds = 100, udtMicroSeconds = 0}
secondsToUnixDiffTime :: Integral a => a -> UnixDiffTime
secondsToUnixDiffTime sec = UnixDiffTime (fromIntegral sec) 0
{-# INLINE secondsToUnixDiffTime #-}

-- | Creating difference from micro seconds.
--
-- >>> microSecondsToUnixDiffTime (12345678 :: Int)
-- UnixDiffTime {udtSeconds = 12, udtMicroSeconds = 345678}
--
-- >>> microSecondsToUnixDiffTime ((-12345678) :: Int)
-- UnixDiffTime {udtSeconds = -12, udtMicroSeconds = -345678}
--
-- >>> microSecondsToUnixDiffTime ((-345678) :: Int)
-- UnixDiffTime {udtSeconds = 0, udtMicroSeconds = -345678}
microSecondsToUnixDiffTime :: Integral a => a -> UnixDiffTime
microSecondsToUnixDiffTime = fromMicro . toInteger
{-# INLINE microSecondsToUnixDiffTime #-}

----------------------------------------------------------------

toFractional :: Fractional a => UnixDiffTime -> a
toFractional (UnixDiffTime s u) = realToFrac s + realToFrac u / 1000000
{-# SPECIALIZE toFractional :: UnixDiffTime -> Double #-}
