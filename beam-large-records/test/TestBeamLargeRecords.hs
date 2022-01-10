module Main (main) where

import Test.Tasty

import qualified Test.Record.Beam.Andres
import qualified Test.Record.Beam.SimpleSQL
import qualified Test.Record.Beam.Tutorial1
import qualified Test.Record.Beam.Tutorial2
import qualified Test.Record.Beam.Tutorial3
import qualified Test.Record.Beam.Zipping

main :: IO ()
main = defaultMain $ testGroup "beam-large-records" [
      Test.Record.Beam.Andres.tests
    , Test.Record.Beam.SimpleSQL.tests
    , Test.Record.Beam.Tutorial1.tests
    , Test.Record.Beam.Tutorial2.tests
    , Test.Record.Beam.Tutorial3.tests
    , Test.Record.Beam.Zipping.tests
    ]