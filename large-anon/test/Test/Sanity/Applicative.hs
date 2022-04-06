{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Applicative (tests) where

import Validation

import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Sanity.Applicative" [
      testCase "insertA" test_insertA
    ]

test_insertA :: Assertion
test_insertA =
    case example of
      Failure _ -> error "unexpected failure"
      Success r -> assertEqual "" r validated

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

example :: Validation [String] (Record [ "a" := Int, "b" := Bool, "c" := Char ])
example =
      Anon.insertA #a validateInt
    $ Anon.insertA #b validateBool
    $ Anon.insertA #c validateChar
    $ pure Anon.empty
  where
    validateInt  :: Validation [String] Int
    validateBool :: Validation [String] Bool
    validateChar :: Validation [String] Char

    validateInt  = Success 1
    validateBool = Success True
    validateChar = Success 'a'

validated :: Record [ "a" := Int, "b" := Bool, "c" := Char ]
validated =
      Anon.insert #a 1
    $ Anon.insert #b True
    $ Anon.insert #c 'a'
    $ Anon.empty
