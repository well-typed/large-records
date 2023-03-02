{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 902

module Test.Sanity.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testCaseInfo "Test.Sanity.OverloadedRecordUpdate" $
      return "Skipped for ghc < 9.2"

#else

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Overloading

import qualified Data.Record.Anon.Simple   as S
import qualified Data.Record.Anon.Advanced as A

tests :: TestTree
tests = testGroup "Test.Sanity.OverloadedRecordUpdate" [
      testGroup "Simple" [
          testCase "topLevel" test_simple_topLevel
        , testCase "nested"   test_simple_nested
        , testCase "punned"   test_simple_punned
        ]
    , testGroup "Advanced" [
          testCase "topLevel" test_advanced_topLevel
        , testCase "nested"   test_advanced_nested
        , testCase "punned"   test_advanced_punned
        ]
    ]

{-------------------------------------------------------------------------------
  Simple API
-------------------------------------------------------------------------------}

test_simple_topLevel :: Assertion
test_simple_topLevel = do
    assertEqual "" expected $
      r { a = 6 }
  where
    r, expected :: S.Record [ "a" := Int, "b" := Bool ]
    r        = ANON { a = 5, b = True }
    expected = ANON { a = 6, b = True }

-- NOTE: nested updates rely on OverloadedRecordDot in addition to
-- OverloadedRecordUpdate.
test_simple_nested :: Assertion
test_simple_nested = do
    assertEqual "" expected $
      r{b.c = False}
  where
    r, expected :: S.Record [ "a" := Int, "b" := S.Record [ "c" := Bool, "d" := Char ] ]
    r        = ANON { a = 5, b = ANON { c = True,  d = 'a' } }
    expected = ANON { a = 5, b = ANON { c = False, d = 'a' } }

test_simple_punned :: Assertion
test_simple_punned =
    assertEqual "" expected $
      let c = False in r{b.c}
  where
    r, expected :: S.Record [ "a" := Int, "b" := S.Record [ "c" := Bool, "d" := Char ] ]
    r        = ANON { a = 5, b = ANON { c = True,  d = 'a' } }
    expected = ANON { a = 5, b = ANON { c = False, d = 'a' } }

{-------------------------------------------------------------------------------
  Advanced API
-------------------------------------------------------------------------------}

test_advanced_topLevel :: Assertion
test_advanced_topLevel = do
    assertEqual "" expected $
      r { a = Nothing }
  where
    r, expected :: A.Record Maybe [ "a" := Int, "b" := Bool ]
    r        = ANON_F { a = Just 5,  b = Just True }
    expected = ANON_F { a = Nothing, b = Just True }

test_advanced_nested :: Assertion
test_advanced_nested = do
    assertEqual "" expected $
      r{b.d = Just 'a'}
  where
    r, expected :: S.Record [ "a" := Int, "b" := A.Record Maybe [ "c" := Bool, "d" := Char ] ]
    r        = ANON { a = 5, b = ANON_F { c = Just True, d = Nothing  } }
    expected = ANON { a = 5, b = ANON_F { c = Just True, d = Just 'a' } }

test_advanced_punned :: Assertion
test_advanced_punned = do
    assertEqual "" expected $
      let d = Just 'a' in r{b.d}
  where
    r, expected :: S.Record [ "a" := Int, "b" := A.Record Maybe [ "c" := Bool, "d" := Char ] ]
    r        = ANON { a = 5, b = ANON_F { c = Just True, d = Nothing  } }
    expected = ANON { a = 5, b = ANON_F { c = Just True, d = Just 'a' } }

#endif