{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnixTimeSpec (main, spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UnixTime
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import System.Environment (lookupEnv)
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
        a <- choose (0, 4294967295) :: Gen Int
        b <- choose (0, 999999) :: Gen Int
        let ut =
                UnixTime
                    { utSeconds = abs (fromIntegral a)
                    , utMicroSeconds = fromIntegral b
                    }
        return ut

spec :: Spec
spec = do
    describe "formatUnixTimeGMT (long output)" $
        it "does not truncate output longer than the initial buffer" $ do
            let fmt = BS.concat (replicate 12 "%Y-%m-%d ")
            formatUnixTimeGMT fmt (UnixTime 0 0)
                `shouldBe` BS.concat (replicate 12 "1970-01-01 ")

    describe "formatUnixTimeGMT (concurrency)" $
        it "does not corrupt TZ when called concurrently" $ do
            tz0 <- lookupEnv "TZ"
            vs <- replicateM 8 newEmptyMVar
            forM_ (zip [0 ..] vs) $ \(i, v) -> forkIO $ do
                forM_ [1 .. 2000 :: Int] $ \j ->
                    BS.length (formatUnixTimeGMT webDateFormat (UnixTime (i * 100000 + fromIntegral j) 0))
                        `seq` return ()
                putMVar v ()
            mapM_ takeMVar vs
            lookupEnv "TZ" `shouldReturn` tz0

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
            let dt = formatUnixTimeGMT webDateFormat ut
                ut' = parseUnixTimeGMT webDateFormat dt
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

    describe "getUnixTime" $ do
        it "works well" $ do
            x <- getUnixTime
            x `shouldBe` x
        it "should work in Template Haskell" $
            $( do
                time <- TH.runIO getUnixTime
                let b = time == time
                [|b|]
             )
                `shouldBe` True

formatMailModel :: UTCTime -> TimeZone -> ByteString
formatMailModel ut zone = BS.pack $ formatTime defaultTimeLocale fmt zt
  where
    zt = utcToZonedTime zone ut
    fmt = BS.unpack mailDateFormat

toUTCTime :: UnixTime -> UTCTime
toUTCTime = posixSecondsToUTCTime . realToFrac . toEpochTime
