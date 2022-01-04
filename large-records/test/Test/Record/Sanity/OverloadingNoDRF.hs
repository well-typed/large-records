{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.OverloadingNoDRF (
    tests
  ) where

import GHC.Records.Compat

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Simple test case

  We don't support DRF-style overloading, but we _can_ generate records when DRF
  is in use, provided all overloading is resolved through `HasField` instead of
  through DRF.
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
  data X = MkX { a :: Int    }
  data Y = MkY { a :: String }
  |]

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
