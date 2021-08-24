{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.OverloadingNoDRF (
    tests
  ) where

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
    assertEqual "X" x.a 0
    assertEqual "Y" y.a "hi"
  where
    x :: X
    x = [lr| MkX {a = 0} |]

    y :: Y
    y = [lr| MkY {a = "hi"} |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.OverloadingNoDRF" [
      testCase "overloading" testOverloading
    ]
