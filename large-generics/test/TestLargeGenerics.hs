module Main where

import Test.Tasty

import qualified Test.Record.Generic.Prop.Show
import qualified Test.Record.Generic.Prop.ToFromJSON
import qualified Test.Record.Generic.Sanity.GhcGenerics
import qualified Test.Record.Generic.Sanity.Laziness
import qualified Test.Record.Generic.Sanity.Lens.VL
import qualified Test.Record.Generic.Sanity.Rep
import qualified Test.Record.Generic.Sanity.Transform

main :: IO ()
main = defaultMain $ testGroup "TestLargeGenerics" [
      Test.Record.Generic.Sanity.Rep.tests
    , Test.Record.Generic.Sanity.Transform.tests
    , Test.Record.Generic.Sanity.GhcGenerics.tests
    , Test.Record.Generic.Sanity.Laziness.tests
    , Test.Record.Generic.Sanity.Lens.VL.tests
    , Test.Record.Generic.Prop.Show.tests
    , Test.Record.Generic.Prop.ToFromJSON.tests
    ]