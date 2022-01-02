module Test.Record.Generic.Prop.ToFromJSON (tests) where

import Data.Aeson.Types (parseEither)

import Data.Record.Generic.JSON

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Record.Generic.Infra.Examples

tests :: TestTree
tests = testGroup "Test.Record.Prop.ToFromJSON" [
      testProperty "tofromJSON" prop_tofromJSON
    ]

-- | Test that gtoJSON and gfromJSON are inverse
prop_tofromJSON :: SimpleRecord -> Property
prop_tofromJSON ex =
      counterexample (show (gtoJSON ex))
    $ Right ex === parseEither gparseJSON (gtoJSON ex)