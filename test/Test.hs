{-# LANGUAGE OverloadedStrings #-}

{-
  % runghc -i.. Test.hs
-}

module Test where

import qualified Data.ByteString.Char8 as BS
import Data.UnixTime
import System.Locale
import System.Time hiding (toClockTime)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

tests :: [Test]
tests = [
    testGroup "Conf" [
         testCase "formatUnixTime" test_formatUnixTime
       , testCase "formatUnixTimeGMT" test_formatUnixTimeGMT
       , testCase "parseUnixTime" test_parseUnixTime
       , testCase "parseUnixTime2" test_parseUnixTime2
       , testCase "formatParse" test_formatParse
       , testCase "fromClockTime" test_fromClockTime
       , testCase "toClockTime" test_toClockTime
       , testCase "diffTime" test_diffTime
       , testCase "diffTimeFromSeconds" test_diffTimeFromSeconds
       ]
  ]

----------------------------------------------------------------

test_formatUnixTime :: Assertion
test_formatUnixTime = do
    res @?= ans
  where
    res = formatUnixTime mailDateFormat $ UnixTime 0 0
    ans = "Thu, 01 Jan 1970 09:00:00 +0900"

test_formatUnixTimeGMT :: Assertion
test_formatUnixTimeGMT = do
    res @?= ans
  where
    res = formatUnixTimeGMT webDateFormat $ UnixTime 0 0
    ans = "Thu, 01 Jan 1970 00:00:00 GMT"

test_parseUnixTime :: Assertion
test_parseUnixTime = do
    res @?= ans
  where
    res = parseUnixTime mailDateFormat "Thu, 01 Jan 1970 09:00:00 +0900"
    ans = UnixTime 0 0

test_parseUnixTime2 :: Assertion
test_parseUnixTime2 = do
    res @?= ans
  where
    res = parseUnixTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
    ans = UnixTime 0 0

test_formatParse :: Assertion
test_formatParse = do
    ut@(UnixTime sec _) <- getUnixTime
    let ut' = parseUnixTime mailDateFormat $ formatUnixTime mailDateFormat ut
    ut' @?= UnixTime sec 0

----------------------------------------------------------------

test_fromClockTime :: Assertion
test_fromClockTime = do
    ct <- getClockTime
    let fmt1 = formatUnixTime "%a, %d %b %Y %H:%M:%S" $ fromClockTime ct
    cal <- toCalendarTime ct
    let fmt2 = BS.pack $ formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" cal
    fmt1 @?= fmt2

test_toClockTime :: Assertion
test_toClockTime = do
    ut <- getUnixTime
    let fmt1 = formatUnixTime "%a, %d %b %Y %H:%M:%S" ut
        ct = toClockTime ut
    cal <- toCalendarTime ct
    let fmt2 = BS.pack $ formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" cal
    fmt1 @?= fmt2

----------------------------------------------------------------

test_diffTime :: Assertion
test_diffTime = do
    ut0 <- getUnixTime
    ut1 <- getUnixTime
    let ut0' = addUnixDiffTime ut1 $ diffUnixTime ut0 ut1
        ut1' = addUnixDiffTime ut0 $ diffUnixTime ut1 ut0
    ut0' @?= ut0
    ut1' @?= ut1

----------------------------------------------------------------

test_diffTimeFromSeconds :: Assertion
test_diffTimeFromSeconds = do
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

main :: Assertion
main = defaultMain tests
