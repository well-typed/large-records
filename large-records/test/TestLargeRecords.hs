module Main (main) where

import Test.Tasty

import qualified Test.Record.Sanity.CodeGen
import qualified Test.Record.Sanity.Derive
import qualified Test.Record.Sanity.EqualFieldTypes
import qualified Test.Record.Sanity.GhcGenerics
import qualified Test.Record.Sanity.HigherKinded
import qualified Test.Record.Sanity.HKD
import qualified Test.Record.Sanity.OverloadedRecordUpdate
import qualified Test.Record.Sanity.OverloadingNoDRF
import qualified Test.Record.Sanity.PatternMatch
import qualified Test.Record.Sanity.QualifiedImports
import qualified Test.Record.Sanity.Optics.SingleModule
import qualified Test.Record.Sanity.Optics.SplitModule
import qualified Test.Record.Sanity.RDP.SingleModule
import qualified Test.Record.Sanity.RDP.SplitModule
import qualified Test.Record.Sanity.RecordConstruction
import qualified Test.Record.Sanity.Strictness
import qualified Test.Record.Sanity.StrictnessStrictData

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      testGroup "Sanity" [
          Test.Record.Sanity.CodeGen.tests
        , Test.Record.Sanity.Derive.tests
        , Test.Record.Sanity.EqualFieldTypes.tests
        , Test.Record.Sanity.HigherKinded.tests
        , Test.Record.Sanity.HKD.tests
        , Test.Record.Sanity.OverloadingNoDRF.tests
        , Test.Record.Sanity.PatternMatch.tests
        , Test.Record.Sanity.QualifiedImports.tests
        , Test.Record.Sanity.Optics.SplitModule.tests
        , Test.Record.Sanity.Optics.SingleModule.tests
        , Test.Record.Sanity.RDP.SplitModule.tests
        , Test.Record.Sanity.RDP.SingleModule.tests
        , Test.Record.Sanity.RecordConstruction.tests
        , Test.Record.Sanity.Strictness.tests
        , Test.Record.Sanity.StrictnessStrictData.tests
        , Test.Record.Sanity.GhcGenerics.tests
        , Test.Record.Sanity.OverloadedRecordUpdate.tests
        ]
    ]
