{-# LANGUAGE OverloadedStrings #-}

module UnixTimeSpec where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Data.UnixTime
import System.Locale
import Test.Hspec

utcTime0 :: UTCTime
utcTime0 = UTCTime {
    utctDay = ModifiedJulianDay 40587
  , utctDayTime = secondsToDiffTime 0
  }

unixTime0 :: UnixTime
unixTime0 = UnixTime 0 0

{- timezone cannot be parsed by some strptime_l().
jst1970 :: ByteString
jst1970 = "Thu, 01 Jan 1970 09:00:00 +0900"
-}

spec :: Spec
spec = do
    describe "formatUnixTime" $
        it "behaves like the model with utcTime0" $ do
            ans <- formatMailModel utcTime0
            formatUnixTime mailDateFormat unixTime0 `shouldBe` ans

{-
    describe "parseUnixTime" $
        it "parses jst1970 properly" $
            parseUnixTime mailDateFormat jst1970 `shouldBe` unixTime0
-}

    describe "parseUnixTimeGMT & formatUnixTimeGMT" $
        it "inverses the result" $ do
            ut@(UnixTime sec _) <- getUnixTime
            let dt  = formatUnixTimeGMT webDateFormat ut
                ut' = parseUnixTimeGMT  webDateFormat dt
                dt' = formatUnixTimeGMT webDateFormat ut'
            ut' `shouldBe` UnixTime sec 0
            dt `shouldBe` dt'

    describe "addUnixDiffTime & diffUnixTime" $
        it "invrses the result" $ do
            ut0 <- getUnixTime
            ut1 <- getUnixTime
            let ut0' = addUnixDiffTime ut1 $ diffUnixTime ut0 ut1
                ut1' = addUnixDiffTime ut0 $ diffUnixTime ut1 ut0
            ut0' `shouldBe` ut0
            ut1' `shouldBe` ut1

formatMailModel :: UTCTime -> IO BS.ByteString
formatMailModel ut = ans <$> getCurrentTimeZone
  where
   toZoneTime tz = utcToZonedTime tz ut
   fmt = BS.unpack mailDateFormat
   ans tz = BS.pack $ formatTime defaultTimeLocale fmt $ toZoneTime tz
