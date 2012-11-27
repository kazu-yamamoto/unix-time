module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-XOverloadedStrings", "dist/build/cbits/conv.o", "Data/UnixTime.hs"]
