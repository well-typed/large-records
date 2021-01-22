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
import qualified Test.Record.Generic.Size.Before.R020 as Before020
import qualified Test.Record.Generic.Size.Before.R030 as Before030
import qualified Test.Record.Generic.Size.Before.R040 as Before040
import qualified Test.Record.Generic.Size.Before.R050 as Before050
import qualified Test.Record.Generic.Size.Before.R060 as Before060
import qualified Test.Record.Generic.Size.Before.R070 as Before070
import qualified Test.Record.Generic.Size.Before.R080 as Before080
import qualified Test.Record.Generic.Size.Before.R090 as Before090
import qualified Test.Record.Generic.Size.Before.R100 as Before100
import qualified Test.Record.Generic.Size.After.R010 as After010
import qualified Test.Record.Generic.Size.After.R020 as After020
import qualified Test.Record.Generic.Size.After.R030 as After030
import qualified Test.Record.Generic.Size.After.R040 as After040
import qualified Test.Record.Generic.Size.After.R050 as After050
import qualified Test.Record.Generic.Size.After.R060 as After060
import qualified Test.Record.Generic.Size.After.R070 as After070
import qualified Test.Record.Generic.Size.After.R080 as After080
import qualified Test.Record.Generic.Size.After.R090 as After090
import qualified Test.Record.Generic.Size.After.R100 as After100

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
    assertEqual "20"
      (toJSON (SOP.glowerBound :: Before020.R))
      (toJSON ( LR.glowerBound ::  After020.R))
    assertEqual "30"
      (toJSON (SOP.glowerBound :: Before030.R))
      (toJSON ( LR.glowerBound ::  After030.R))
    assertEqual "40"
      (toJSON (SOP.glowerBound :: Before040.R))
      (toJSON ( LR.glowerBound ::  After040.R))
    assertEqual "50"
      (toJSON (SOP.glowerBound :: Before050.R))
      (toJSON ( LR.glowerBound ::  After050.R))
    assertEqual "60"
      (toJSON (SOP.glowerBound :: Before060.R))
      (toJSON ( LR.glowerBound ::  After060.R))
    assertEqual "70"
      (toJSON (SOP.glowerBound :: Before070.R))
      (toJSON ( LR.glowerBound ::  After070.R))
    assertEqual "80"
      (toJSON (SOP.glowerBound :: Before080.R))
      (toJSON ( LR.glowerBound ::  After080.R))
    assertEqual "90"
      (toJSON (SOP.glowerBound :: Before090.R))
      (toJSON ( LR.glowerBound ::  After090.R))
    assertEqual "100"
      (toJSON (SOP.glowerBound :: Before100.R))
      (toJSON ( LR.glowerBound ::  After100.R))

-- | Test that we have the necessary @HasField@ instances
test_sameField :: Assertion
test_sameField = do
    assertEqual "HasField"
      sop010.field1
      lr010.field1
    assertEqual "HasField"
      sop020.field11
      lr020.field11
    assertEqual "HasField"
      sop030.field21
      lr030.field21
    assertEqual "HasField"
      sop040.field31
      lr040.field31
    assertEqual "HasField"
      sop050.field41
      lr050.field41
    assertEqual "HasField"
      sop060.field51
      lr060.field51
    assertEqual "HasField"
      sop070.field61
      lr070.field61
    assertEqual "HasField"
      sop080.field71
      lr080.field71
    assertEqual "HasField"
      sop090.field81
      lr090.field81
    assertEqual "HasField"
      sop100.field91
      lr100.field91
  where
    sop010 = SOP.glowerBound :: Before010.R
    sop020 = SOP.glowerBound :: Before020.R
    sop030 = SOP.glowerBound :: Before030.R
    sop040 = SOP.glowerBound :: Before040.R
    sop050 = SOP.glowerBound :: Before050.R
    sop060 = SOP.glowerBound :: Before060.R
    sop070 = SOP.glowerBound :: Before070.R
    sop080 = SOP.glowerBound :: Before080.R
    sop090 = SOP.glowerBound :: Before090.R
    sop100 = SOP.glowerBound :: Before100.R

    lr010 = LR.glowerBound :: After010.R
    lr020 = LR.glowerBound :: After020.R
    lr030 = LR.glowerBound :: After030.R
    lr040 = LR.glowerBound :: After040.R
    lr050 = LR.glowerBound :: After050.R
    lr060 = LR.glowerBound :: After060.R
    lr070 = LR.glowerBound :: After070.R
    lr080 = LR.glowerBound :: After080.R
    lr090 = LR.glowerBound :: After090.R
    lr100 = LR.glowerBound :: After100.R
