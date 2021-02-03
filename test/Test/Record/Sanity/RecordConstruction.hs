{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wwarn #-}

module Test.Record.Sanity.RecordConstruction (tests) where

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

-- Test that this works if we don't generate field accessors
-- However, set fields to lazy for this test, so that we can test with
-- missing fields.
largeRecord (defaultPureScript {allFieldsStrict = False}) [d|
    data R = MkR { x :: Int, y :: Bool }
  |]

-- This call just indicates to @ghc@ that we have reached the end of a binding
-- group, and so it should process all definitions. This is not necessary if
--
-- * There is another call to 'largeRecord' (or any other TH splice) in between
--   the record definition and its use
-- * The record definition and the record use are in different modules.
--
-- TODO: It'd be nicer if we could avoid this altogether.
endOfBindingGroup

inOrder :: R
inOrder = [mkRecord| MkR { x = 1234, y = True } |]

outOfOrder :: R
outOfOrder = [mkRecord| MkR { y = True, x = 1234 } |]

-- Results in "Unexpected fields" error
-- extraFields :: R
-- extraFields = [mkRecord| MkR { x = 1234, y = True, z = () } |]

-- But this works (with a warning)
missingFields :: R
missingFields = [mkRecord| MkR { x = 1234 } |]

{-------------------------------------------------------------------------------
  Sanity check
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RecordConstruction" [
      testCase "allEqual" testAllEqual
    ]

testAllEqual :: Assertion
testAllEqual = do
    assertEqual "inOrder/outOfOrder"    inOrder.x outOfOrder.x
    assertEqual "inOrder/missingFields" inOrder.x missingFields.x
