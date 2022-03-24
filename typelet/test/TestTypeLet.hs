module Main (main) where

import Test.Tasty

import qualified Test.Sanity

main :: IO ()
main = defaultMain $ testGroup "TestTypeLet" [
      Test.Sanity.tests
    ]
