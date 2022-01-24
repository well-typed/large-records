{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.DuplicateFields (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous

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
    actual :: Record '[ '("a", Bool) ]
    actual = castRecord $ insert #a True $ insert #a False $ empty

    expected :: Record '[ '("a", Bool) ]
    expected = insert #a True $ empty

test_insertDifferentType :: Assertion
test_insertDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record '[ '("a", Bool) ]
    actual = castRecord $ insert #a True $ insert #a 'a' $ empty

    expected :: Record '[ '("a", Bool) ]
    expected = insert #a True $ empty

test_mergeSameType :: Assertion
test_mergeSameType = do
    assertEqual "" expected actual
  where
    actual :: Record '[ '("a", Bool) ]
    actual = castRecord $ merge (insert #a True empty) (insert #a False empty)

    expected :: Record '[ '("a", Bool) ]
    expected = insert #a True $ empty

test_mergeDifferentType :: Assertion
test_mergeDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record '[ '("a", Bool) ]
    actual = castRecord $ merge (insert #a True empty) (insert #a 'a' empty)

    expected :: Record '[ '("a", Bool) ]
    expected = insert #a True $ empty
