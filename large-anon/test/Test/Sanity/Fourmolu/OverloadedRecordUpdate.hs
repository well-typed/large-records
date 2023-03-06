{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 902

module Test.Sanity.Fourmolu.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testCaseInfo "Test.Sanity.Fourmolu.OverloadedRecordUpdate" $
      return "Skipped for ghc < 9.2"

#else

-- | Test with Fourmolu, without RDP
--
-- See "Test.Sanity.Fourmolu.OverloadedRecordDot" for additional discussion.
{-# OPTIONS_GHC -F -pgmF=large-anon-testsuite-fourmolu-preprocessor #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Fourmolu.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Overloading
import Data.Record.Anon.Simple

tests :: TestTree
tests = testGroup "Test.Sanity.Fourmolu.OverloadedRecordUpdate" [
      testCase "definition" test_definition
    , testGroup "Simple" [
          testCase "get" test_simple_get
        , testCase "set" test_simple_set
        ]
    , testGroup "Nested" [
          testCase "get" test_nested_get
        , testCase "set" test_nested_set
        ]
    ]

test_definition :: Assertion
test_definition = do
    assertEqual "" expected $ show r
  where
    r :: Record [ "a" := Int, "b" := Bool ]
    r = ANON { a = 5, b = True }

    expected :: String
    expected = "ANON {a = 5, b = True}"

test_simple_get :: Assertion
test_simple_get =
    -- Without OverloadedRecordDot, fourmolu turns this into @r . b@
    assertEqual "" True $ r.b
  where
    r :: Record [ "a" := Int, "b" := Bool ]
    r = ANON { a = 5, b = True }

test_simple_set :: Assertion
test_simple_set = do
    assertEqual "" expected $
      -- record-dot-preprocessor doesn't want any whitespace in @r{a@
      -- but fortunately that is precisely the syntax that fourmolu generates
      r{a = 6}
  where
    r, expected :: Record [ "a" := Int, "b" := Bool ]
    r        = ANON { a = 5, b = True }
    expected = ANON { a = 6, b = True }

test_nested_get :: Assertion
test_nested_get =
    assertEqual "" 'x' $ r.b.d
  where
    r :: Record [ "a" := Int, "b" := Record [ "c" := Bool, "d" := Char ] ]
    r = ANON { a = 5, b = ANON { c = True, d = 'x' } }

test_nested_set :: Assertion
test_nested_set = do
    -- fourmolu will parse this as "illegal overloaded record update"
    -- when OverloadedRecordUpdate is not enabled.
    assertEqual "" expected $
      r{b.c = False}
  where
    r, expected :: Record [ "a" := Int, "b" := Record [ "c" := Bool, "d" := Char ] ]
    r        = ANON { a = 5, b = ANON { c = True,  d = 'a' } }
    expected = ANON { a = 5, b = ANON { c = False, d = 'a' } }

#endif