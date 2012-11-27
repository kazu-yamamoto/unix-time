module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-XOverloadedStrings"
  , "-idist/build"
  , "dist/build/cbits/conv.o"
  , "Data/UnixTime.hs"
  ]
