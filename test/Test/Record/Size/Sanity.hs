{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- | Sanity checks that we generate correct code for in the @Sized.*@ modules
--
-- The real test of the @Size.*@ moduels of course is not this module, but
-- rather their compiled size.
module Test.Record.Size.Sanity (tests) where

import Data.Aeson
import Data.Functor.Identity

import qualified Data.Record.Generic.SOP        as SOP
import qualified Data.Record.Generic.LowerBound as LR

import qualified Test.Record.Size.Before.R010 as Before010
import qualified Test.Record.Size.After.R0010 as After0010
import qualified Test.Record.Size.After.HK010 as AfterHK10

#if PROFILE_GEN_CODE
import Test.Record.Size.Infra (T(..))

import qualified Test.Record.Size.Before.R020 as Before020
import qualified Test.Record.Size.Before.R030 as Before030
import qualified Test.Record.Size.Before.R040 as Before040
import qualified Test.Record.Size.Before.R050 as Before050
import qualified Test.Record.Size.Before.R060 as Before060
import qualified Test.Record.Size.Before.R070 as Before070
import qualified Test.Record.Size.Before.R080 as Before080
import qualified Test.Record.Size.Before.R090 as Before090
import qualified Test.Record.Size.Before.R100 as Before100
import qualified Test.Record.Size.After.R0020 as After0020
import qualified Test.Record.Size.After.R0030 as After0030
import qualified Test.Record.Size.After.R0040 as After0040
import qualified Test.Record.Size.After.R0050 as After0050
import qualified Test.Record.Size.After.R0060 as After0060
import qualified Test.Record.Size.After.R0070 as After0070
import qualified Test.Record.Size.After.R0080 as After0080
import qualified Test.Record.Size.After.R0090 as After0090
import qualified Test.Record.Size.After.R0100 as After0100
import qualified Test.Record.Size.After.R0200 as After0200
import qualified Test.Record.Size.After.R0300 as After0300
import qualified Test.Record.Size.After.R0400 as After0400
import qualified Test.Record.Size.After.R0500 as After0500
import qualified Test.Record.Size.After.R0600 as After0600
import qualified Test.Record.Size.After.R0700 as After0700
import qualified Test.Record.Size.After.R0800 as After0800
import qualified Test.Record.Size.After.R0900 as After0900
import qualified Test.Record.Size.After.R1000 as After1000
import qualified Test.Record.Size.After.HK020 as AfterHK20
import qualified Test.Record.Size.After.HK030 as AfterHK30
import qualified Test.Record.Size.After.HK040 as AfterHK40
import qualified Test.Record.Size.After.HK050 as AfterHK50
import qualified Test.Record.Size.After.HK060 as AfterHK60
import qualified Test.Record.Size.After.HK070 as AfterHK70
import qualified Test.Record.Size.After.HK080 as AfterHK80
import qualified Test.Record.Size.After.HK090 as AfterHK90
import qualified Test.Record.Size.After.HK100 as AfterH100
#endif

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Size.Sanity" [
      testCase "sameValue" test_sameValue
    , testCase "sameField" test_sameField
    , testCase "showHK"    test_showHK
#if PROFILE_GEN_CODE
    , testCase "reallyBig" test_reallyBig
#endif
    ]

-- | Test that @generics-sop@ and @large-records@ generates the same structure
test_sameValue :: Assertion
test_sameValue = do
    assertEqual "10"
      (toJSON (SOP.glowerBound :: Before010.R))
      (toJSON ( LR.glowerBound :: After0010.R))
#if PROFILE_GEN_CODE
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
#endif

-- | Test that we have the necessary @HasField@ instances
test_sameField :: Assertion
test_sameField = do
    assertEqual "HasField" sop010.field1  lr010.field1
#if PROFILE_GEN_CODE
    assertEqual "HasField" sop020.field11 lr020.field11
    assertEqual "HasField" sop030.field21 lr030.field21
    assertEqual "HasField" sop040.field31 lr040.field31
    assertEqual "HasField" sop050.field41 lr050.field41
    assertEqual "HasField" sop060.field51 lr060.field51
    assertEqual "HasField" sop070.field61 lr070.field61
    assertEqual "HasField" sop080.field71 lr080.field71
    assertEqual "HasField" sop090.field81 lr090.field81
    assertEqual "HasField" sop100.field91 lr100.field91
#endif
  where
    sop010 = SOP.glowerBound :: Before010.R
    lr010  =  LR.glowerBound :: After0010.R

#if PROFILE_GEN_CODE
    sop020 = SOP.glowerBound :: Before020.R
    sop030 = SOP.glowerBound :: Before030.R
    sop040 = SOP.glowerBound :: Before040.R
    sop050 = SOP.glowerBound :: Before050.R
    sop060 = SOP.glowerBound :: Before060.R
    sop070 = SOP.glowerBound :: Before070.R
    sop080 = SOP.glowerBound :: Before080.R
    sop090 = SOP.glowerBound :: Before090.R
    sop100 = SOP.glowerBound :: Before100.R

    lr020 = LR.glowerBound :: After0020.R
    lr030 = LR.glowerBound :: After0030.R
    lr040 = LR.glowerBound :: After0040.R
    lr050 = LR.glowerBound :: After0050.R
    lr060 = LR.glowerBound :: After0060.R
    lr070 = LR.glowerBound :: After0070.R
    lr080 = LR.glowerBound :: After0080.R
    lr090 = LR.glowerBound :: After0090.R
    lr100 = LR.glowerBound :: After0100.R
#endif

-- | Check that we can 'Show' the higher-kinded records
test_showHK :: Assertion
test_showHK = do
    assertBool "some output" (not . null $ show lr010)

#if PROFILE_GEN_CODE
    assertBool "some output" (not . null $ show lr020)
    assertBool "some output" (not . null $ show lr030)
    assertBool "some output" (not . null $ show lr040)
    assertBool "some output" (not . null $ show lr050)
    assertBool "some output" (not . null $ show lr060)
    assertBool "some output" (not . null $ show lr070)
    assertBool "some output" (not . null $ show lr080)
    assertBool "some output" (not . null $ show lr090)
    assertBool "some output" (not . null $ show lr100)
#endif
  where
    lr010 = LR.glowerBound :: AfterHK10.HKR Identity

#if PROFILE_GEN_CODE
    lr020 = LR.glowerBound :: AfterHK20.HKR Identity
    lr030 = LR.glowerBound :: AfterHK30.HKR Identity
    lr040 = LR.glowerBound :: AfterHK40.HKR Identity
    lr050 = LR.glowerBound :: AfterHK50.HKR Identity
    lr060 = LR.glowerBound :: AfterHK60.HKR Identity
    lr070 = LR.glowerBound :: AfterHK70.HKR Identity
    lr080 = LR.glowerBound :: AfterHK80.HKR Identity
    lr090 = LR.glowerBound :: AfterHK90.HKR Identity
    lr100 = LR.glowerBound :: AfterH100.HKR Identity
#endif

#if PROFILE_GEN_CODE
-- | Check the value of the last field in the truly big records
test_reallyBig :: Assertion
test_reallyBig = do
    assertEqual "HasField" lr0100.field100  (MkT  100)
    assertEqual "HasField" lr0200.field200  (MkT  200)
    assertEqual "HasField" lr0300.field300  (MkT  300)
    assertEqual "HasField" lr0400.field400  (MkT  400)
    assertEqual "HasField" lr0500.field500  (MkT  500)
    assertEqual "HasField" lr0600.field600  (MkT  600)
    assertEqual "HasField" lr0700.field700  (MkT  700)
    assertEqual "HasField" lr0800.field800  (MkT  800)
    assertEqual "HasField" lr0900.field900  (MkT  900)
    assertEqual "HasField" lr1000.field1000 (MkT 1000)
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
#endif
