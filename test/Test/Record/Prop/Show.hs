module Test.Record.Prop.Show (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Test.Record.Prop.Show.Regular as Regular
import qualified Test.Record.Prop.Show.Large   as Large

tests :: TestTree
tests = testGroup "Test.Record.Prop.Show" [
      testProperty "show" prop_show
    ]

prop_show :: Regular.Example1 -> Property
prop_show ex = show ex === show (Large.fromRegular ex)
