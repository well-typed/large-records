module Main (main) where

import Test.Tasty

import qualified Test.Prop.Record.Combinators.Constrained
import qualified Test.Prop.Record.Combinators.Simple
import qualified Test.Prop.Record.Diff
import qualified Test.Sanity.AllFields
import qualified Test.Sanity.Applicative
import qualified Test.Sanity.BlogPost
import qualified Test.Sanity.CheckIsSubRow
import qualified Test.Sanity.Discovery
import qualified Test.Sanity.DuplicateFields
import qualified Test.Sanity.Generics
import qualified Test.Sanity.HasField
import qualified Test.Sanity.Intersection
import qualified Test.Sanity.Merging
import qualified Test.Sanity.OverloadedRecordDot
import qualified Test.Sanity.OverloadedRecordUpdate
import qualified Test.Sanity.PolyKinds
import qualified Test.Sanity.RebindableSyntax.Disabled
import qualified Test.Sanity.RebindableSyntax.Enabled
import qualified Test.Sanity.RecordLens
import qualified Test.Sanity.Regression
import qualified Test.Sanity.Simple
import qualified Test.Sanity.SrcPlugin.WithoutTypelet
import qualified Test.Sanity.SrcPlugin.WithTypelet
import qualified Test.Sanity.TypeLevelMetadata

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      testGroup "Sanity" [
          Test.Sanity.HasField.tests
        , Test.Sanity.Generics.tests
        , Test.Sanity.Merging.tests
        , Test.Sanity.RecordLens.tests
        , Test.Sanity.DuplicateFields.tests
        , Test.Sanity.TypeLevelMetadata.tests
        , Test.Sanity.AllFields.tests
        , Test.Sanity.Applicative.tests
        , Test.Sanity.Simple.tests
        , Test.Sanity.PolyKinds.tests
        , Test.Sanity.CheckIsSubRow.tests
        , Test.Sanity.Discovery.tests
        , Test.Sanity.SrcPlugin.WithoutTypelet.tests
        , Test.Sanity.SrcPlugin.WithTypelet.tests
        , Test.Sanity.Intersection.tests
        , Test.Sanity.BlogPost.tests
        , Test.Sanity.OverloadedRecordDot.tests
        , Test.Sanity.OverloadedRecordUpdate.tests
        , Test.Sanity.RebindableSyntax.Disabled.tests
        , Test.Sanity.RebindableSyntax.Enabled.tests
        , Test.Sanity.Regression.tests
        ]
    , testGroup "Prop" [
          Test.Prop.Record.Diff.tests
        , Test.Prop.Record.Combinators.Simple.tests
        , Test.Prop.Record.Combinators.Constrained.tests
        ]
    ]