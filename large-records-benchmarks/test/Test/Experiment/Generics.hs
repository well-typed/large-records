{-# LANGUAGE OverloadedStrings #-}

module Test.Experiment.Generics (tests) where

import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson.Types as Aeson

import qualified Experiment.Generics_SOP as SOP
import qualified Experiment.Generics_LR  as LR

import Bench.HList

{-------------------------------------------------------------------------------
  Sanity check that the instance works
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Experiment.Generics" [
      testCase "LR"  test_LR
    , testCase "SOP" test_SOP
    ]

test_LR :: Assertion
test_LR = do
    assertEqual "0" (object val0) $ LR.gtoJSON exampleRec0
    assertEqual "1" (object val1) $ LR.gtoJSON exampleRec1
    assertEqual "2" (object val2) $ LR.gtoJSON exampleRec2
    assertEqual "3" (object val3) $ LR.gtoJSON exampleRec3
    assertEqual "4" (object val4) $ LR.gtoJSON exampleRec4
    assertEqual "5" (object val5) $ LR.gtoJSON exampleRec5
  where
    val0 :: [Aeson.Pair]
    val0 = []
    val1 = "Int"    .= (1234 :: Int)    : val0
    val2 = "Char"   .= ('x'  :: Char)   : val1
    val3 = "String" .= ("yz" :: String) : val2
    val4 = "Double" .= (5.6  :: Double) : val3
    val5 = "Bool"   .= (True :: Bool)   : val4

-- | This is the same test as 'test_LR', but using 'SOP'
--
-- We could abstract out 'gtoJSON' but it requires a bit of context.
test_SOP :: Assertion
test_SOP = do
    assertEqual "0" (object val0) $ SOP.gtoJSON exampleRec0
    assertEqual "1" (object val1) $ SOP.gtoJSON exampleRec1
    assertEqual "2" (object val2) $ SOP.gtoJSON exampleRec2
    assertEqual "3" (object val3) $ SOP.gtoJSON exampleRec3
    assertEqual "4" (object val4) $ SOP.gtoJSON exampleRec4
    assertEqual "5" (object val5) $ SOP.gtoJSON exampleRec5
  where
    val0 :: [Aeson.Pair]
    val0 = []
    val1 = "Int"    .= (1234 :: Int)    : val0
    val2 = "Char"   .= ('x'  :: Char)   : val1
    val3 = "String" .= ("yz" :: String) : val2
    val4 = "Double" .= (5.6  :: Double) : val3
    val5 = "Bool"   .= (True :: Bool)   : val4

