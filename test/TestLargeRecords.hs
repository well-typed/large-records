module Main (main) where

import Test.Tasty

import qualified Test.Record.Generic.Sanity.Generics
import qualified Test.Record.Generic.Sanity.OverloadingNoDRF
import qualified Test.Record.Generic.Size.Sanity
import qualified Test.Record.Generic.Strictness

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      Test.Record.Generic.Sanity.Generics.tests
    , Test.Record.Generic.Sanity.OverloadingNoDRF.tests
    , Test.Record.Generic.Strictness.tests
    , Test.Record.Generic.Size.Sanity.tests
    ]
