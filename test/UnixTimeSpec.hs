{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnixTimeSpec (main, spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM, unless)
import Data.Binary (decode, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UnixTime
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Info (os)
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

-- Normalized values only, as created by the library.
instance Arbitrary UnixDiffTime where
    arbitrary =
        microSecondsToUnixDiffTime
            <$> (choose (-10000000000000, 10000000000000) :: Gen Integer)

spec :: Spec
spec = do
    describe "parseUnixTime (DST)" $
        it "takes DST into account" $ unless (os == "mingw32") $ do
            tz0 <- lookupEnv "TZ"
            setEnv "TZ" "EST5EDT"
            let fmt = "%Y-%m-%d %H:%M:%S"
            utSeconds (parseUnixTime fmt "2024-07-01 12:00:00") `shouldBe` 1719849600
            utSeconds (parseUnixTime fmt "2024-01-01 12:00:00") `shouldBe` 1704128400
            maybe (unsetEnv "TZ") (setEnv "TZ") tz0
            -- mktime() calls tzset() to reload TZ.
            utSeconds (parseUnixTime fmt "2000-01-01 00:00:00") `seq` return ()

    describe "parse with %z" $ do
        it "parseUnixTimeGMT honors the offset" $
            parseUnixTimeGMT mailDateFormat "Thu, 01 Jan 1970 09:00:00 +0900"
                `shouldBe` UnixTime 0 0
        it "parseUnixTime honors the offset" $
            parseUnixTime mailDateFormat "Wed, 31 Dec 1969 19:00:00 -0500"
                `shouldBe` UnixTime 0 0

    describe "parseUnixTimeMaybe & parseUnixTimeGMTMaybe" $ do
        it "returns Just on success" $ do
            parseUnixTimeGMTMaybe webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
                `shouldBe` Just (UnixTime 0 0)
            parseUnixTimeMaybe mailDateFormat "Thu, 01 Jan 1970 09:00:00 +0900"
                `shouldBe` Just (UnixTime 0 0)
        it "returns Nothing on failure" $ do
            parseUnixTimeGMTMaybe webDateFormat "garbage" `shouldBe` Nothing
            parseUnixTimeMaybe "%Y-%m-%d" "garbage" `shouldBe` Nothing
        it "returns Nothing if the input is not entirely consumed" $
            parseUnixTimeGMTMaybe "%Y-%m-%d" "1970-01-01 junk" `shouldBe` Nothing
        it "returns Nothing for NUL bytes" $
            parseUnixTimeGMTMaybe "%Y-%m-%d" "1970-01-01\0junk" `shouldBe` Nothing

    describe "formatUnixTimeGMT (zone)" $
        it "formats %s and %z in UTC" $
            formatUnixTimeGMT "%s %z %H" (UnixTime 1234567890 0)
                `shouldBe` "1234567890 +0000 23"

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

    describe "UnixDiffTime Num instance" $ do
        let r :: UnixDiffTime -> Rational
            r = toRational
            normalized (UnixDiffTime sec usec) =
                abs usec < 1000000
                    && not (sec > 0 && usec < 0)
                    && not (sec < 0 && usec > 0)
            same x v = normalized x && r x == v
        prop "+" $ \x y -> same (x + y) (r x + r y)
        prop "-" $ \x y -> same (x - y) (r x - r y)
        prop "*" $ \x y ->
            same (x * y) (fromInteger (truncate (r x * r y * 1000000)) / 1000000)
        prop "negate" $ \x -> same (negate x) (negate (r x))
        prop "abs" $ \x -> same (abs x) (abs (r x))
        prop "signum" $ \x -> same (signum x) (signum (r x))
        prop "compare" $ \x y -> compare x y == compare (r x) (r y)
        prop "diffUnixTime" $ \ut0 ut1 ->
            same (diffUnixTime ut0 ut1) (utR ut0 - utR ut1)

    describe "UnixTime Binary instance" $ do
        prop "decode . encode = id" $ \ut ->
            decode (encode ut) == (ut :: UnixTime)
        it "encodes seconds as Int64" $
            BL.length (encode (UnixTime 0 0)) `shouldBe` 12

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

utR :: UnixTime -> Rational
utR (UnixTime sec usec) = toRational sec + toRational usec / 1000000

toUTCTime :: UnixTime -> UTCTime
toUTCTime = posixSecondsToUTCTime . realToFrac . toEpochTime
