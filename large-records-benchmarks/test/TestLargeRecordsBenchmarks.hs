module Main (main) where

import Test.Tasty

import qualified Test.After
import qualified Test.HigherKinded
import qualified Test.Experiment.Generics

-- | Sanity tests for the benchmarks
main :: IO ()
main = defaultMain $ testGroup "TestLargeRecordsBenchmarks" [
      Test.After.tests
    , Test.HigherKinded.tests
    , Test.Experiment.Generics.tests
    ]

