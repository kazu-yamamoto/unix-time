#!/bin/sh

sed -i~ '/^Library/i\
Custom-Setup { Setup-Depends: base, Cabal >= 2, cabal-doctest >=1.0.6 && <1.1 }
' unix-time.cabal
sed -i~ 's/^Build-Type:.*$/Build-Type: Custom/' unix-time.cabal
sed -i~ 's/Buildable: *False/Buildable: True/' unix-time.cabal

cat > Setup.hs <<EOF
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest ( defaultMainAutoconfWithDoctests )
main :: IO ()
main = defaultMainAutoconfWithDoctests "doctests"

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
#warning You are configuring this package without cabal-doctest installed. \
         The doctests test-suite will not work as a result. \
         To fix this, install cabal-doctest before configuring.
#endif

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks

#endif
EOF
