module Main (main) where

import Test.Tasty

import qualified Test.Sanity.HasField
import qualified Test.Sanity.Generics
import qualified Test.Sanity.Merging
import qualified Test.Sanity.Casting
import qualified Test.Sanity.DuplicateFields
import qualified Test.Sanity.TypeLevelMetadata
import qualified Test.Sanity.AllFields
import qualified Test.Sanity.Simple
import qualified Test.Prop.Record.Combinators.Simple
import qualified Test.Prop.Record.Combinators.Constrained

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      testGroup "Sanity" [
          Test.Sanity.HasField.tests
        , Test.Sanity.Generics.tests
        , Test.Sanity.Merging.tests
        , Test.Sanity.Casting.tests
        , Test.Sanity.DuplicateFields.tests
        , Test.Sanity.TypeLevelMetadata.tests
        , Test.Sanity.AllFields.tests
        , Test.Sanity.Simple.tests
        ]
    , testGroup "Prop" [
          Test.Prop.Record.Combinators.Simple.tests
        , Test.Prop.Record.Combinators.Constrained.tests
        ]
    ]