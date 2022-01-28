module Main (main) where

import Test.Tasty

import qualified Test.Record.Anonymous.Sanity.HasField
import qualified Test.Record.Anonymous.Sanity.Generics
import qualified Test.Record.Anonymous.Sanity.Merging
import qualified Test.Record.Anonymous.Sanity.Casting
import qualified Test.Record.Anonymous.Sanity.DuplicateFields
import qualified Test.Record.Anonymous.Sanity.TypeLevelMetadata
import qualified Test.Record.Anonymous.Sanity.RecordDicts
import qualified Test.Record.Anonymous.Prop.Combinators.Simple
import qualified Test.Record.Anonymous.Prop.Combinators.Constrained

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      testGroup "Sanity" [
          Test.Record.Anonymous.Sanity.HasField.tests
        , Test.Record.Anonymous.Sanity.Generics.tests
        , Test.Record.Anonymous.Sanity.Merging.tests
        , Test.Record.Anonymous.Sanity.Casting.tests
        , Test.Record.Anonymous.Sanity.DuplicateFields.tests
        , Test.Record.Anonymous.Sanity.TypeLevelMetadata.tests
        , Test.Record.Anonymous.Sanity.RecordDicts.tests
        ]
    , testGroup "Prop" [
          Test.Record.Anonymous.Prop.Combinators.Simple.tests
        , Test.Record.Anonymous.Prop.Combinators.Constrained.tests
        ]
    ]