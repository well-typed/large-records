{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Test.Record.Sanity.ErrorsAndWarnings (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Record.Util
import Test.Record.Sanity.ErrorsAndWarnings.Stage1

problemsForSyntaxErrorInExp :: (Maybe String, [Problem])
problemsForSyntaxErrorInExp = $qProblemsForSyntaxErrorInExp

problemsForSyntaxErrorInPat :: (Maybe String, [Problem])
problemsForSyntaxErrorInPat = $qProblemsForSyntaxErrorInPat

problemsForUnknownFieldsInExp :: (Maybe String, [Problem])
problemsForUnknownFieldsInExp = $qProblemsForUnknownFieldsInExp

problemsForUnknownFieldsInPat :: (Maybe String, [Problem])
problemsForUnknownFieldsInPat = $qProblemsForUnknownFieldsInPat

problemsForMissingFieldsInExp :: (Maybe String, [Problem])
problemsForMissingFieldsInExp = $qProblemsForMissingFieldsInExp

problemsForMissingFieldsInPat :: (Maybe String, [Problem])
problemsForMissingFieldsInPat = $qProblemsForMissingFieldsInPat

tests :: TestTree
tests = testGroup "Test.Record.Sanity.ErrorsAndWarnings" [
      testCase "problemsForSyntaxErrorInExp" test_problemsForSyntaxErrorInExp
    , testCase "problemsForSyntaxErrorInPat" test_problemsForSyntaxErrorInPat
    , testCase "problemsForUnknownFieldsInExp" test_problemsForUnknownFieldsInExp
    , testCase "problemsForUnknownFieldsInPat" test_problemsForUnknownFieldsInPat
    , testCase "problemsForMissingFieldsInExp" test_problemsForMissingFieldsInExp
    , testCase "problemsForMissingFieldsInPat" test_problemsForMissingFieldsInPat
    ]

test_problemsForSyntaxErrorInExp :: Assertion
test_problemsForSyntaxErrorInExp = do
    assertJust "no failure reported" (fst problemsForSyntaxErrorInExp) $
      assertPrefix "Could not parse expression:"
    assertEqual "other problems" [] (snd problemsForSyntaxErrorInExp)

test_problemsForSyntaxErrorInPat :: Assertion
test_problemsForSyntaxErrorInPat = do
    assertJust "no failure reported" (fst problemsForSyntaxErrorInPat) $
      assertPrefix "Could not parse pattern:"
    assertEqual "other problems" [] (snd problemsForSyntaxErrorInPat)

test_problemsForUnknownFieldsInExp :: Assertion
test_problemsForUnknownFieldsInExp =
    assertEqual "" expected problemsForUnknownFieldsInExp
  where
    expected :: (Maybe String, [Problem])
    expected = (Nothing, [Error "Unknown fields: c"])

test_problemsForUnknownFieldsInPat :: Assertion
test_problemsForUnknownFieldsInPat =
    assertEqual "" expected problemsForUnknownFieldsInPat
  where
    expected :: (Maybe String, [Problem])
    expected = (Nothing, [Error "Unknown fields: c"])

test_problemsForMissingFieldsInExp :: Assertion
test_problemsForMissingFieldsInExp =
    assertEqual "" expected problemsForMissingFieldsInExp
  where
    expected :: (Maybe String, [Problem])
    expected = (Nothing, [Warning "No value for field b"])

test_problemsForMissingFieldsInPat :: Assertion
test_problemsForMissingFieldsInPat =
    assertEqual "" expected problemsForMissingFieldsInPat
  where
    -- It is of course perfectly fine for a pattern to be missing fields
    expected :: (Maybe String, [Problem])
    expected = (Nothing, [])

