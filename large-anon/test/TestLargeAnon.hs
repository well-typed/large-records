module Main (main) where

import Test.Tasty

import qualified Test.Record.Anonymous.Sanity.Basics
import qualified Test.Record.Anonymous.Sanity.Merging

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      Test.Record.Anonymous.Sanity.Basics.tests
    , Test.Record.Anonymous.Sanity.Merging.tests
    ]
