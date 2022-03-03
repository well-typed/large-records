{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Test.After (tests) where

import Data.Aeson
import GHC.Records.Compat

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Record.Generic.SOP        as SOP
import qualified Data.Record.Generic.LowerBound as LR

import Bench.Types

import qualified Before.Sized.R010 as Before010
import qualified Before.Sized.R020 as Before020
import qualified Before.Sized.R030 as Before030
import qualified Before.Sized.R040 as Before040
import qualified Before.Sized.R050 as Before050
import qualified Before.Sized.R060 as Before060
import qualified Before.Sized.R070 as Before070
import qualified Before.Sized.R080 as Before080
import qualified Before.Sized.R090 as Before090
import qualified Before.Sized.R100 as Before100

import qualified After.Sized.R0010 as After0010
import qualified After.Sized.R0020 as After0020
import qualified After.Sized.R0030 as After0030
import qualified After.Sized.R0040 as After0040
import qualified After.Sized.R0050 as After0050
import qualified After.Sized.R0060 as After0060
import qualified After.Sized.R0070 as After0070
import qualified After.Sized.R0080 as After0080
import qualified After.Sized.R0090 as After0090
import qualified After.Sized.R0100 as After0100
import qualified After.Sized.R0200 as After0200
import qualified After.Sized.R0300 as After0300
import qualified After.Sized.R0400 as After0400
import qualified After.Sized.R0500 as After0500
import qualified After.Sized.R0600 as After0600
import qualified After.Sized.R0700 as After0700
import qualified After.Sized.R0800 as After0800
import qualified After.Sized.R0900 as After0900
import qualified After.Sized.R1000 as After1000

tests :: TestTree
tests = testGroup "Test.After" [
      testCase "likeWithLike" test_likeWithLike
    , testCase "reallyBig"    test_reallyBig
    , testCase "hasField"     test_hasField
    ]

-- | Test that we are comparing like with like: compare JSON
--
-- We cannot do this test for the truly large examples, since we do not
-- define such large records in the " Before " benchmark.
test_likeWithLike :: Assertion
test_likeWithLike = do
    assertEqual "10"
      (toJSON (SOP.glowerBound :: Before010.R))
      (toJSON ( LR.glowerBound :: After0010.R))
    assertEqual "20"
      (toJSON (SOP.glowerBound :: Before020.R))
      (toJSON ( LR.glowerBound :: After0020.R))
    assertEqual "30"
      (toJSON (SOP.glowerBound :: Before030.R))
      (toJSON ( LR.glowerBound :: After0030.R))
    assertEqual "40"
      (toJSON (SOP.glowerBound :: Before040.R))
      (toJSON ( LR.glowerBound :: After0040.R))
    assertEqual "50"
      (toJSON (SOP.glowerBound :: Before050.R))
      (toJSON ( LR.glowerBound :: After0050.R))
    assertEqual "60"
      (toJSON (SOP.glowerBound :: Before060.R))
      (toJSON ( LR.glowerBound :: After0060.R))
    assertEqual "70"
      (toJSON (SOP.glowerBound :: Before070.R))
      (toJSON ( LR.glowerBound :: After0070.R))
    assertEqual "80"
      (toJSON (SOP.glowerBound :: Before080.R))
      (toJSON ( LR.glowerBound :: After0080.R))
    assertEqual "90"
      (toJSON (SOP.glowerBound :: Before090.R))
      (toJSON ( LR.glowerBound :: After0090.R))
    assertEqual "100"
      (toJSON (SOP.glowerBound :: Before100.R))
      (toJSON ( LR.glowerBound :: After0100.R))

-- | Test that the truly large records indeed are that large
--
-- We verify this by checking the value of the last field.
test_reallyBig :: Assertion
test_reallyBig = do
    assertEqual "HasField" (getField @"field100"  lr0100) (MkT  100)
    assertEqual "HasField" (getField @"field200"  lr0200) (MkT  200)
    assertEqual "HasField" (getField @"field300"  lr0300) (MkT  300)
    assertEqual "HasField" (getField @"field400"  lr0400) (MkT  400)
    assertEqual "HasField" (getField @"field500"  lr0500) (MkT  500)
    assertEqual "HasField" (getField @"field600"  lr0600) (MkT  600)
    assertEqual "HasField" (getField @"field700"  lr0700) (MkT  700)
    assertEqual "HasField" (getField @"field800"  lr0800) (MkT  800)
    assertEqual "HasField" (getField @"field900"  lr0900) (MkT  900)
    assertEqual "HasField" (getField @"field1000" lr1000) (MkT 1000)
  where
    lr0100 = LR.glowerBound :: After0100.R
    lr0200 = LR.glowerBound :: After0200.R
    lr0300 = LR.glowerBound :: After0300.R
    lr0400 = LR.glowerBound :: After0400.R
    lr0500 = LR.glowerBound :: After0500.R
    lr0600 = LR.glowerBound :: After0600.R
    lr0700 = LR.glowerBound :: After0700.R
    lr0800 = LR.glowerBound :: After0800.R
    lr0900 = LR.glowerBound :: After0900.R
    lr1000 = LR.glowerBound :: After1000.R

-- | Test that we have the necessary @HasField@ instances
test_hasField :: Assertion
test_hasField = do
    assertEqual "HasField" (getField @"field1"  sop010) (getField @"field1"  lr010)
    assertEqual "HasField" (getField @"field11" sop020) (getField @"field11" lr020)
    assertEqual "HasField" (getField @"field21" sop030) (getField @"field21" lr030)
    assertEqual "HasField" (getField @"field31" sop040) (getField @"field31" lr040)
    assertEqual "HasField" (getField @"field41" sop050) (getField @"field41" lr050)
    assertEqual "HasField" (getField @"field51" sop060) (getField @"field51" lr060)
    assertEqual "HasField" (getField @"field61" sop070) (getField @"field61" lr070)
    assertEqual "HasField" (getField @"field71" sop080) (getField @"field71" lr080)
    assertEqual "HasField" (getField @"field81" sop090) (getField @"field81" lr090)
    assertEqual "HasField" (getField @"field91" sop100) (getField @"field91" lr100)
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

    lr010 = LR.glowerBound :: After0010.R
    lr020 = LR.glowerBound :: After0020.R
    lr030 = LR.glowerBound :: After0030.R
    lr040 = LR.glowerBound :: After0040.R
    lr050 = LR.glowerBound :: After0050.R
    lr060 = LR.glowerBound :: After0060.R
    lr070 = LR.glowerBound :: After0070.R
    lr080 = LR.glowerBound :: After0080.R
    lr090 = LR.glowerBound :: After0090.R
    lr100 = LR.glowerBound :: After0100.R


