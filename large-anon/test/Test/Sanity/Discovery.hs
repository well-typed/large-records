{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Discovery (tests) where

import Data.Either (fromRight)
import Data.Record.Generic

import Data.Record.Anon

import qualified Data.Record.Anon.Simple   as S
import qualified Data.Record.Anon.Advanced as A

import Test.Tasty
import Test.Tasty.HUnit

import Test.Infra.Discovery
import Test.Infra.DynRecord
import Test.Infra.MarkStrictness

import qualified Test.Infra.DynRecord.Simple   as Dyn.S
import qualified Test.Infra.DynRecord.Advanced as Dyn.A

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Discovery" [
      testGroup "Simple" [
          testCase "inferType" test_simple_inferType
        , testCase "toLens"    test_simple_toLens
        ]
    , testGroup "Advanced" [
          testCase "inferType" test_advanced_inferType
        , testCase "toLens"    test_advanced_toLens
        ]
    ]

{-------------------------------------------------------------------------------
  Tests for the simple API (kind Type)
-------------------------------------------------------------------------------}

type ExpectedSimple = [ "a" := Int, "c" := Char ]

test_simple_inferType :: Assertion
test_simple_inferType =
    case Dyn.S.inferType example1 of
      Dyn.S.SomeRecord r -> do
        assertEqual "show" expected $
          show r

        -- The comparison test implies that we are parsing one record, then
        -- using the result to parse another /in the same shape/
        assertEqual "compare" (Right r) $
          Dyn.S.toRecord r example2
  where
    expected :: String
    expected = "Record {a = 1, b = True, c = 'a'}"

test_simple_toLens :: Assertion
test_simple_toLens = do
    assertEqual "get" expectedGet $
      show get
    assertEqual "set" example1' $
      set (S.set #c 'b' get)
    assertEqual "missingField" expectedMissingField . fmap (const ()) $
      Dyn.S.toLens (Proxy @ExpectedSimple) exampleMissingField
    assertEqual "wrongType" expectedWrongType  . fmap (const ()) $
      Dyn.S.toLens (Proxy @ExpectedSimple) exampleWrongType
  where
    get :: S.Record ExpectedSimple
    set :: S.Record ExpectedSimple -> DynRecord
    (get, set) =
        fromRight (error "unexpected error") $
          Dyn.S.toLens (Proxy @ExpectedSimple) example1

    expectedGet :: String
    expectedGet = "Record {a = 1, c = 'a'}"

    expectedMissingField :: Either NotSubRow ()
    expectedMissingField = Left ["c"]

    -- TODO: For now we don't get more information in case of a type error
    expectedWrongType :: Either NotSubRow ()
    expectedWrongType = Left ["c"]

{-------------------------------------------------------------------------------
  Tests for the advanced API (kind other than Type)

  These follow the same structure as the tests for the simple API. We don't
  explicitly test the error cases again here ('NotSubRow'), since the
  projection and parsing machinery is the same for the simple and the advanced
  case.
-------------------------------------------------------------------------------}

type ExpectedAdvanced = [ "a" := Strict Int, "c" := Lazy Char ]

test_advanced_inferType :: Assertion
test_advanced_inferType =
    case Dyn.A.inferType example1 :: Dyn.A.SomeRecord Boxed of
      Dyn.A.SomeRecord r -> do
        assertEqual "show" expected $
          show r

        -- The comparison test implies that we are parsing one record, then
        -- using the result to parse another /in the same shape/
        assertEqual "compare" (Right r) $
          Dyn.A.toRecord r example2
  where
    expected :: String
    expected = "Record {a = 1, b = True, c = 'a'}"

test_advanced_toLens :: Assertion
test_advanced_toLens = do
    assertEqual "get" expectedGet $
      show get
    assertEqual "set" example1' $
      set (A.set #c (BoxLazy 'b') get)
  where
    get :: A.Record Boxed ExpectedAdvanced
    set :: A.Record Boxed ExpectedAdvanced -> DynRecord
    (get, set) =
       fromRight (error "unexpected error") $
         Dyn.A.toLens (Proxy @ExpectedAdvanced) example1

    expectedGet :: String
    expectedGet = "Record {a = 1, c = 'a'}"

{-------------------------------------------------------------------------------
  Example 'DynRecord' values

  These are used both for the simple and the advanced tests (which of course
  interpret them differently).

  TODO: We should have a test with some shadowing.
  TODO: We should not have all fields alphabetical.
-------------------------------------------------------------------------------}

-- | Main running example
example1 :: DynRecord
example1 = DynRecord [
      ("a", VI 1)
    , ("b", VB True)
    , ("c", VC 'a')
    ]

-- | Like 'example1', but one field updated
example1' :: DynRecord
example1' = DynRecord [
      ("a", VI 1)
    , ("b", VB True)
    , ("c", VC 'b')
    ]

-- | Equal to 'example1', but with some additional fields
--
-- We should be able to parse this according to the type inferred for
-- 'example1'.
example2 :: DynRecord
example2 = DynRecord [
      ("a", VI 1)
    , ("b", VB True)
    , ("c", VC 'a')
    , ("d", VI 2)
    ]

-- | Example that does not conform to 'Expected': field missing
exampleMissingField :: DynRecord
exampleMissingField = DynRecord [
      ("a", VI 1)
    ]

-- | Example that does not conform to 'Expected': wrong field type
exampleWrongType :: DynRecord
exampleWrongType = DynRecord [
      ("a", VI 1)
    , ("b", VB True)
    , ("c", VB False)
    ]

