{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if USE_RDP
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
#endif

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.OverloadingNoDRF (
    tests
  ) where

#if !USE_RDP
import GHC.Records.Compat
#endif

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Simple test case
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
  data X = MkX { a :: Int    }
  data Y = MkY { a :: String }
  |]

testOverloading :: Assertion
testOverloading = do
#if USE_RDP
    assertEqual "X" x.a 0
    assertEqual "Y" y.a "hi"
#else
    assertEqual "X" (getField @"a" x) 0
    assertEqual "Y" (getField @"a" y) "hi"
#endif
  where
    x :: X
    x = [lr| MkX {a = 0} |]

    y :: Y
    y = [lr| MkY {a = "hi"} |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.OverloadingNoDRF" [
      testCase "overloading" testOverloading
    ]
