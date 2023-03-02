{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 902

module Test.Sanity.Fourmolu.OverloadedRecordDot (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testCaseInfo "Test.Sanity.Fourmolu.OverloadedRecordDot" $
      return "Skipped for ghc < 9.2"

#else

-- | Test with Fourmolu, using RDP
--
-- We use fourmolu as a preprocessor. This is obviously a weird usage of a
-- formatter, but the point here is to ensure that large-anon /can/ be used
-- with fourmolu, without it changing the code in incorrect ways.
--
-- To manually check the output of Fourmolu, use
--
-- > cabal run fourmolu-preprocessor x test/Test/Sanity/Fourmolu/OverloadedRecordDot.hs /dev/stdout
{-# OPTIONS_GHC -F -pgmF=fourmolu-preprocessor #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Fourmolu.OverloadedRecordDot (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Simple

tests :: TestTree
tests = testGroup "Test.Sanity.Fourmolu.OverloadedRecordDot" [
      testCase "definition" test_definition
    , testGroup "Simple" [
          testCase "get" test_simple_get
        , testCase "set" test_simple_set
        ]
    , testGroup "Nested" [
          testCase "get" test_nested_get
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

#endif