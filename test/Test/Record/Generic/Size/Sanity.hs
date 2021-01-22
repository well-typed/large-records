{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- | Sanity checks that we generate correct code for in the @Sized.*@ modules
--
-- The real test of the @Size.*@ moduels of course is not this module, but
-- rather their compiled size.
module Test.Record.Generic.Size.Sanity (tests) where

import Data.Aeson

import qualified Data.Record.Generic.SOP        as SOP
import qualified Data.Record.Generic.LowerBound as LR

import qualified Test.Record.Generic.Size.Before.R010 as Before010
import qualified Test.Record.Generic.Size.After.R010 as After010

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Generic.Size.Sanity" [
      testCase "sameValue" test_sameValue
    , testCase "sameField" test_sameField
    ]

-- | Test that @generics-sop@ and @large-records@ generates the same structure
test_sameValue :: Assertion
test_sameValue = do
    assertEqual "10"
      (toJSON (SOP.glowerBound :: Before010.R))
      (toJSON ( LR.glowerBound ::  After010.R))

-- | Test that we have the necessary @HasField@ instances
test_sameField :: Assertion
test_sameField = do
    assertEqual "HasField"
      sop.field1
      lr.field1
  where
    sop :: Before010.R
    sop = SOP.glowerBound

    lr :: After010.R
    lr = LR.glowerBound
