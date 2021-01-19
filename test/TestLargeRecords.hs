module Main (main) where

import Test.Tasty

import qualified Data.Record.Generic.Sanity

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
      Data.Record.Generic.Sanity.tests
    ]
