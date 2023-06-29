{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
{-# LANGUAGE OverloadedLabels #-}

module Test.Sanity.Regression (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon.Simple

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Sets for specific bugs
tests :: TestTree
tests = testGroup "Test.Sanity.Regression" [
      testGroup "issue146" [
          testCase "get_insert_anon"                      test_get_insert_anon
        , testCase "get_insert_applyPending_insert_empty" test_get_insert_applyPending_insert_empty
        , testCase "get_insert_insert_empty"              test_get_insert_insert_empty
        , testCase "get_applyPending_insert_empty"        test_get_applyPending_insert_empty
        , testCase "get_insert_empty"                     test_get_insert_empty
        ]
    ]

{-------------------------------------------------------------------------------
  Issue #146
-------------------------------------------------------------------------------}

-- | The issue as reported
--
-- The bug caused this test to segfault.
test_get_insert_anon :: Assertion
test_get_insert_anon =
    assertEqual "" "field1" $
        get #field1
      $ insert #field2 "field2"
      $ ANON { field1 = "field1" }

-- | Manual expansion of ANON
--
-- The bug caused this test to segfault.
test_get_insert_applyPending_insert_empty :: Assertion
test_get_insert_applyPending_insert_empty =
    assertEqual "" "field1" $
        get #field1
      $ insert #field2 "field2"
      $ applyPending
      $ insert #field1 "field1"
      $ empty

-- | Omit call to 'applyPending'
--
-- This test was not affected by the bug.
test_get_insert_insert_empty :: Assertion
test_get_insert_insert_empty =
    assertEqual "" "field1" $
        get #field1
      $ insert #field2 "field2"
      $ insert #field1 "field1"
      $ empty

-- | Only single insert, but still call applyPending
--
-- This test was not affected by the bug.
test_get_applyPending_insert_empty :: Assertion
test_get_applyPending_insert_empty =
    assertEqual "" "field1" $
        get #field1
      $ applyPending
      $ insert #field1 "field1"
      $ empty

-- | Simplest form: just a get after an insert
--
-- The bug caused this test to segfault.
test_get_insert_empty :: Assertion
test_get_insert_empty =
    assertEqual "" "field1" $
        get #field1
      $ insert #field1 "field1"
      $ empty
