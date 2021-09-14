{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Experiments.Generic (
    tests

    -- * Exported for the benefit of the size measurement modules
  , gtoJSON
  ) where

import Data.Aeson

#if defined(BLOG2_VARIANT_QUADRATIC) || defined(BLOG2_VARIANT_LOGARITHMIC)
import Data.Record.Generic
import qualified Data.Record.Generic.JSON as LR
#endif
#if defined(BLOG2_VARIANT_SOP)
import Generics.SOP
import qualified Generics.SOP.JSON as SOP
#endif

import qualified Data.Aeson.Types as Aeson

import Test.Tasty
import Test.Tasty.HUnit

import Test.Record.Experiments.HList

#if defined(BLOG2_VARIANT_QUADRATIC) || defined(BLOG2_VARIANT_LOGARITHMIC)
import Test.Record.Experiments.Generic.Instance.LargeRecords ()
#endif
#if defined(BLOG2_VARIANT_SOP)
import Test.Record.Experiments.Generic.Instance.SOP ()
#endif

#if defined(BLOG2_VARIANT_QUADRATIC) || defined(BLOG2_VARIANT_LOGARITHMIC)
gtoJSON :: (Generic a, Constraints a ToJSON) => a -> Value
gtoJSON = LR.gtoJSON
#endif
#if defined(BLOG2_VARIANT_SOP)
gtoJSON :: (HasDatatypeInfo a, All2 ToJSON (Code a)) => a -> Value
gtoJSON = SOP.gtoJSON SOP.defaultJsonOptions
#endif

{-------------------------------------------------------------------------------
  Sanity check that the instance works
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Experiments.Generic" [
      testCase "toJSON" test_toJSON
    ]

test_toJSON :: Assertion
test_toJSON = do
    assertEqual "0" (object val0) $ gtoJSON exampleRec0
    assertEqual "1" (object val1) $ gtoJSON exampleRec1
    assertEqual "2" (object val2) $ gtoJSON exampleRec2
    assertEqual "3" (object val3) $ gtoJSON exampleRec3
    assertEqual "4" (object val4) $ gtoJSON exampleRec4
    assertEqual "5" (object val5) $ gtoJSON exampleRec5
  where
    val0 :: [Aeson.Pair]
    val0 = []
    val1 = "Int"    .= (1234 :: Int)    : val0
    val2 = "Char"   .= ('x'  :: Char)   : val1
    val3 = "String" .= ("yz" :: String) : val2
    val4 = "Double" .= (5.6  :: Double) : val3
    val5 = "Bool"   .= (True :: Bool)   : val4


