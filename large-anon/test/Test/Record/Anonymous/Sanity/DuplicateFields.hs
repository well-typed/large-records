{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.DuplicateFields (tests) where

import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.DuplicateFields" [
      testCase "insertSameType"      test_insertSameType
    , testCase "insertDifferentType" test_insertDifferentType
    , testCase "mergeSameType"       test_mergeSameType
    , testCase "mergeDifferentType"  test_mergeDifferentType
    ]

test_insertSameType :: Assertion
test_insertSameType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
                 Anon.insert #a (I True)
               $ Anon.insert #a (I False)
               $ Anon.empty

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_insertDifferentType :: Assertion
test_insertDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
                 Anon.insert #a (I True)
               $ Anon.insert #a (I 'a')
               $ Anon.empty

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_mergeSameType :: Assertion
test_mergeSameType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
               Anon.merge
                 (Anon.insert #a (I True)  Anon.empty)
                 (Anon.insert #a (I False) Anon.empty)

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_mergeDifferentType :: Assertion
test_mergeDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
               Anon.merge (Anon.insert #a (I True) Anon.empty)
                          (Anon.insert #a (I 'a')  Anon.empty)

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty
