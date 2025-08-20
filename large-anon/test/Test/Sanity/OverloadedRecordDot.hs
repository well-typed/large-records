{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 902

module Test.Sanity.OverloadedRecordDot (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testCaseInfo "Test.Sanity.OverloadedRecordDot" $
      return "Skipped for ghc < 9.2"

#else

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.OverloadedRecordDot (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon

import qualified Data.Record.Anon.Simple   as S
import qualified Data.Record.Anon.Advanced as A

tests :: TestTree
tests = testGroup "Test.Sanity.OverloadedRecordDot" [
      testGroup "Simple" [
          testCase "topLevel" test_simple_topLevel
        , testCase "nested"   test_simple_nested
        ]
    , testGroup "Advanced" [
          testCase "topLevel" test_advanced_topLevel
        , testCase "nested"   test_advanced_nested
        ]
    ]

{-------------------------------------------------------------------------------
  Simple API
-------------------------------------------------------------------------------}

test_simple_topLevel :: Assertion
test_simple_topLevel =
    assertEqual "" True $ r.b
  where
    r :: S.Record [ "a" := Int, "b" := Bool ]
    r = ANON { a = 5, b = True }

test_simple_nested :: Assertion
test_simple_nested =
    assertEqual "" 'x' $ r.b.d
  where
    r :: S.Record [ "a" := Int, "b" := S.Record [ "c" := Bool, "d" := Char ] ]
    r = ANON { a = 5, b = ANON { c = True, d = 'x' } }

{-------------------------------------------------------------------------------
  Advanced API
-------------------------------------------------------------------------------}

test_advanced_topLevel :: Assertion
test_advanced_topLevel =
    assertEqual "" Nothing $ r.b
  where
    r :: A.Record Maybe [ "a" := Int, "b" := Bool ]
    r = ANON_F { a = Just 5, b = Nothing }

-- It only makes sense to consider what happens if we test ANON_F instead
-- ANON: if the outer record is also ANON_F, then we can't just chain record
-- selectors; after all there is some functor in the way now.
test_advanced_nested :: Assertion
test_advanced_nested =
    assertEqual "" "xyz" $ r.b.d
  where
    r :: S.Record [ "a" := Int, "b" := A.Record [] [ "c" := Bool, "d" := Char ] ]
    r = ANON { a = 5, b = ANON_F { c = [], d = "xyz" } }

#endif
