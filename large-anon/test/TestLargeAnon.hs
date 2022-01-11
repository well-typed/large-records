module Main (main) where

import Test.Tasty

import qualified Test.Record.Anonymous.Sanity

main :: IO ()
main = defaultMain $ testGroup "large-anon" [
      Test.Record.Anonymous.Sanity.tests
    ]
