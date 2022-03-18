module Main (main) where

import Test.Tasty

import qualified Test.Prop.Record.Combinators.Constrained
import qualified Test.Prop.Record.Combinators.Simple
import qualified Test.Sanity.AllFields
import qualified Test.Sanity.DuplicateFields
import qualified Test.Sanity.Existential
import qualified Test.Sanity.Generics
import qualified Test.Sanity.HasField
import qualified Test.Sanity.Lens
import qualified Test.Sanity.Merging
import qualified Test.Sanity.Simple
import qualified Test.Sanity.TypeLevelMetadata

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      testGroup "Sanity" [
          Test.Sanity.HasField.tests
        , Test.Sanity.Generics.tests
        , Test.Sanity.Merging.tests
        , Test.Sanity.Lens.tests
        , Test.Sanity.DuplicateFields.tests
        , Test.Sanity.TypeLevelMetadata.tests
        , Test.Sanity.AllFields.tests
        , Test.Sanity.Simple.tests
        , Test.Sanity.Existential.tests
        ]
    , testGroup "Prop" [
          Test.Prop.Record.Combinators.Simple.tests
        , Test.Prop.Record.Combinators.Constrained.tests
        ]
    ]