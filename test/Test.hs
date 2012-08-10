{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{-
  % runghc -i.. Test.hs
-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import System.Locale
import System.Time hiding (toClockTime)
import Test.Framework.Providers.HUnit
import Test.Framework.TH.Prime
import Test.HUnit

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

case_formatUnixTime :: Assertion
case_formatUnixTime = do
    res @?= ans
  where
    res = formatUnixTime mailDateFormat $ UnixTime 0 0
    ans = "Thu, 01 Jan 1970 09:00:00 +0900"

case_formatUnixTimeGMT :: Assertion
case_formatUnixTimeGMT = do
    res @?= ans
  where
    res = formatUnixTimeGMT webDateFormat $ UnixTime 0 0
    ans = "Thu, 01 Jan 1970 00:00:00 GMT"

case_parseUnixTime :: Assertion
case_parseUnixTime = do
    res @?= ans
  where
    res = parseUnixTime mailDateFormat "Thu, 01 Jan 1970 09:00:00 +0900"
    ans = UnixTime 0 0

case_parseUnixTime2 :: Assertion
case_parseUnixTime2 = do
    res @?= ans
  where
    res = parseUnixTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
    ans = UnixTime 0 0

case_formatParse :: Assertion
case_formatParse = do
    ut@(UnixTime sec _) <- getUnixTime
    let ut' = parseUnixTime mailDateFormat $ formatUnixTime mailDateFormat ut
    ut' @?= UnixTime sec 0

----------------------------------------------------------------

case_fromClockTime :: Assertion
case_fromClockTime = do
    ct <- getClockTime
    let fmt1 = formatUnixTime "%a, %d %b %Y %H:%M:%S" $ fromClockTime ct
    cal <- toCalendarTime ct
    let fmt2 = BS.pack $ formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" cal
    fmt1 @?= fmt2

case_toClockTime :: Assertion
case_toClockTime = do
    ut <- getUnixTime
    let fmt1 = formatUnixTime "%a, %d %b %Y %H:%M:%S" ut
        ct = toClockTime ut
    cal <- toCalendarTime ct
    let fmt2 = BS.pack $ formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" cal
    fmt1 @?= fmt2

----------------------------------------------------------------

case_diffTime :: Assertion
case_diffTime = do
    ut0 <- getUnixTime
    ut1 <- getUnixTime
    let ut0' = addUnixDiffTime ut1 $ diffUnixTime ut0 ut1
        ut1' = addUnixDiffTime ut0 $ diffUnixTime ut1 ut0
    ut0' @?= ut0
    ut1' @?= ut1

----------------------------------------------------------------

case_diffTimeFromSeconds :: Assertion
case_diffTimeFromSeconds = do
    res1 @?= ans
    res2 @?= ans
    res3 @?= ans
  where
    base = parseUnixTime mailDateFormat "Tue, 22 Nov 2011 06:49:58 +0900"
    ans = parseUnixTime mailDateFormat "Tue, 22 Nov 2011 06:50:02 +0900"
    res1 = addUnixDiffTime base 4
    res2 = addUnixDiffTime base (secondsToUnixDiffTime 4)
    res3 = addUnixDiffTime base (microSecondsToUnixDiffTime 4000000)

----------------------------------------------------------------

case_diffTimeToSeconds :: Assertion
case_diffTimeToSeconds = do
    res @?= ans
  where
    ans :: Rational
    ans = -12.345678
    res = realToFrac $ microSecondsToUnixDiffTime (-12345678 :: Int)

----------------------------------------------------------------
