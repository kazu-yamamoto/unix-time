{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnixTimeSpec (main, spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UnixTime
import System.Locale (defaultTimeLocale)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec spec

instance Arbitrary UnixTime where
    arbitrary = do
        a <- choose (0,4294967295) :: Gen Int
        b <- choose (0,999999) :: Gen Int
        let ut = UnixTime {
                utSeconds = abs (fromIntegral a)
              , utMicroSeconds = fromIntegral b
              }
        return ut

spec :: Spec
spec = do
    describe "formatUnixTime" $
        prop "behaves like the model" $ \ut -> do
            let ours = formatUnixTime mailDateFormat ut
                utcTime = toUTCTime ut
            timeZone <- getTimeZone utcTime
            let model = formatMailModel utcTime timeZone
            ours `shouldBe` model

    describe "parseUnixTimeGMT & formatUnixTimeGMT" $
        prop "inverses the result" $ \ut@(UnixTime sec _) ->
            let dt  = formatUnixTimeGMT webDateFormat ut
                ut' = parseUnixTimeGMT  webDateFormat dt
                dt' = formatUnixTimeGMT webDateFormat ut'
            in ut' == UnixTime sec 0 && dt == dt'

    describe "addUnixDiffTime & diffUnixTime" $
        prop "invrses the result" $ \(ut0, ut1) ->
            let ut0' = addUnixDiffTime ut1 $ diffUnixTime ut0 ut1
                ut1' = addUnixDiffTime ut0 $ diffUnixTime ut1 ut0
            in ut0' == ut0 && ut1' == ut1

formatMailModel :: UTCTime -> TimeZone -> ByteString
formatMailModel ut zone = BS.pack $ formatTime defaultTimeLocale fmt zt
  where
   zt = utcToZonedTime zone ut
   fmt = BS.unpack mailDateFormat

toUTCTime :: UnixTime -> UTCTime
toUTCTime = posixSecondsToUTCTime . realToFrac . toEpochTime
