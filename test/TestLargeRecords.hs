module Main (main) where

import Test.Tasty

import qualified Test.Record.Prop.ToFromJSON
import qualified Test.Record.Sanity.Derive
import qualified Test.Record.Sanity.Generics
import qualified Test.Record.Sanity.HigherKinded
import qualified Test.Record.Sanity.HKD
import qualified Test.Record.Sanity.OverloadingNoDRF
import qualified Test.Record.Sanity.PatternMatch
import qualified Test.Record.Sanity.QualifiedImports
import qualified Test.Record.Sanity.RecordConstruction
import qualified Test.Record.Sanity.Strictness
import qualified Test.Record.Sanity.Transform
import qualified Test.Record.Size.Sanity

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      testGroup "Sanity" [
          Test.Record.Sanity.Derive.tests
        , Test.Record.Sanity.Generics.tests
        , Test.Record.Sanity.HigherKinded.tests
        , Test.Record.Sanity.HKD.tests
        , Test.Record.Sanity.OverloadingNoDRF.tests
        , Test.Record.Sanity.PatternMatch.tests
        , Test.Record.Sanity.QualifiedImports.tests
        , Test.Record.Sanity.RecordConstruction.tests
        , Test.Record.Sanity.Strictness.tests
        , Test.Record.Sanity.Transform.tests
        ]
    , testGroup "Size" [
          Test.Record.Size.Sanity.tests
        ]
    , testGroup "Prop" [
          Test.Record.Prop.ToFromJSON.tests
        ]
    ]
