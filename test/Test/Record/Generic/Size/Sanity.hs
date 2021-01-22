-- | Sanity checks that we generate correct code for in the @Sized.*@ modules
--
-- The real test of the @Size.*@ moduels of course is not this module, but
-- rather their compiled size.
module Test.Record.Generic.Size.Sanity (tests) where

import Data.Aeson

import qualified Data.Record.Generic.SOP        as SOP
import qualified Data.Record.Generic.LowerBound as LR

import qualified Test.Record.Generic.Size.Before.R010 as B010
import qualified Test.Record.Generic.Size.After.R010 as A010

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Generic.Size.Sanity" [
      testCase "sameValue" test_sameValue
    ]

test_sameValue :: Assertion
test_sameValue = do
    assertEqual "10"
      (toJSON (SOP.glowerBound :: B010.R))
      (toJSON ( LR.glowerBound :: A010.R))
