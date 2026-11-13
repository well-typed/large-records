module Main (main) where

import Test.Tasty

import Test.Prop.Record.Combinators.Constrained qualified
import Test.Prop.Record.Combinators.Simple      qualified
import Test.Prop.Record.Diff                    qualified
import Test.Sanity.AllFields                    qualified
import Test.Sanity.Applicative                  qualified
import Test.Sanity.BlogPost                     qualified
import Test.Sanity.CheckIsSubRow                qualified
import Test.Sanity.Discovery                    qualified
import Test.Sanity.DuplicateFields              qualified
import Test.Sanity.Generics                     qualified
import Test.Sanity.HasField                     qualified
import Test.Sanity.Intersection                 qualified
import Test.Sanity.Merging                      qualified
import Test.Sanity.OverloadedRecordDot          qualified
import Test.Sanity.OverloadedRecordUpdate       qualified
import Test.Sanity.PolyKinds                    qualified
import Test.Sanity.RebindableSyntax.Disabled    qualified
import Test.Sanity.RebindableSyntax.Enabled     qualified
import Test.Sanity.RecordLens                   qualified
import Test.Sanity.Regression                   qualified
import Test.Sanity.Simple                       qualified
import Test.Sanity.SrcPlugin                    qualified
import Test.Sanity.TypeLevelMetadata            qualified

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
        , Test.Sanity.SrcPlugin.tests
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
