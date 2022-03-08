{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.OverloadingNoDRF (
    tests
  ) where

import GHC.Records.Compat

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Simple test case

  We don't support DRF-style overloading, but we _can_ generate records when DRF
  is in use, provided all overloading is resolved through `HasField` instead of
  through DRF.
--------------------------------------s-----------------------------------------}

{-# ANN type X largeRecordStrict #-}
data X = MkX { a :: Int }

{-# ANN type Y largeRecordStrict #-}
data Y = MkY { a :: String }

testOverloading :: Assertion
testOverloading = do
    assertEqual "X" (getField @"a" x) 0
    assertEqual "Y" (getField @"a" y) "hi"
  where
    x :: X
    x = MkX {a = 0}

    y :: Y
    y = MkY {a = "hi"}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.OverloadingNoDRF" [
      testCase "overloading" testOverloading
    ]
