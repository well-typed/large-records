module Test.Record.Generic.Prop.Show (tests) where

import Data.Record.Generic.Show (gshowsPrec)

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Record.Generic.Infra.Examples

tests :: TestTree
tests = testGroup "Test.Record.Generic.Prop.Show" [
      testProperty "show" prop_show
    ]

prop_show :: SimpleRecord -> Property
prop_show ex = show ex === gshowsPrec 0 ex ""
