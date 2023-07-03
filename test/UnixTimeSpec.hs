{-# LANGUAGE OverloadedStrings, FlexibleInstances, CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnixTimeSpec (main, spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UnixTime
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke)
import qualified Language.Haskell.TH as TH (runIO)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((===))

#if !MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
#endif

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
            ours `shouldReturn` model

    describe "parseUnixTimeGMT & formatUnixTimeGMT" $ do
        let (===) = (==) `on` utSeconds
        prop "inverses the result" $ \ut ->
            let dt  = formatUnixTimeGMT webDateFormat ut
                ut' = parseUnixTimeGMT  webDateFormat dt
                dt' = formatUnixTimeGMT webDateFormat ut'
            in ut === ut' && dt == dt'
        prop "inverses the result (2)" $ \ut ->
            let str = formatUnixTimeGMT "%s" ut
                ut' = parseUnixTimeGMT "%s" str
            in ut === ut'

    describe "addUnixDiffTime & diffUnixTime" $
        prop "invrses the result" $ \(ut0, ut1) ->
            let ut0' = addUnixDiffTime ut1 $ diffUnixTime ut0 ut1
                ut1' = addUnixDiffTime ut0 $ diffUnixTime ut1 ut0
            in ut0' == ut0 && ut1' == ut1

    describe "UnixTime Storable instance" $
        prop "peek . poke = id" $ \ut ->
            let pokePeek :: Ptr UnixTime -> IO UnixTime
                pokePeek ptr = poke ptr ut >> peek ptr
            in shouldReturn (alloca pokePeek) ut

    describe "getTimeOfDay" $
        it "should work in Template Haskell" $
            $(do time <- TH.runIO getUnixTime
                 let b = time == time
                 [| b |])
            `shouldBe` True

formatMailModel :: UTCTime -> TimeZone -> ByteString
formatMailModel ut zone = BS.pack $ formatTime defaultTimeLocale fmt zt
  where
   zt = utcToZonedTime zone ut
   fmt = BS.unpack mailDateFormat

toUTCTime :: UnixTime -> UTCTime
toUTCTime = posixSecondsToUTCTime . realToFrac . toEpochTime
