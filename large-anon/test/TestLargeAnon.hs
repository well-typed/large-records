module Main (main) where

import Test.Tasty

import qualified Test.Prop.Record.Combinators.Constrained
import qualified Test.Prop.Record.Combinators.Simple
import qualified Test.Sanity.AllFields
import qualified Test.Sanity.Applicative
import qualified Test.Sanity.CheckCanProject
import qualified Test.Sanity.Discovery
import qualified Test.Sanity.DuplicateFields
import qualified Test.Sanity.Generics
import qualified Test.Sanity.HasField
import qualified Test.Sanity.Intersection
import qualified Test.Sanity.Merging
import qualified Test.Sanity.PolyKinds
import qualified Test.Sanity.RecordLens
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
        , Test.Sanity.CheckCanProject.tests
        , Test.Sanity.Discovery.tests
        , Test.Sanity.SrcPlugin.WithoutTypelet.tests
        , Test.Sanity.SrcPlugin.WithTypelet.tests
        , Test.Sanity.Intersection.tests
        ]
    , testGroup "Prop" [
          Test.Prop.Record.Combinators.Simple.tests
        , Test.Prop.Record.Combinators.Constrained.tests
        ]
    ]