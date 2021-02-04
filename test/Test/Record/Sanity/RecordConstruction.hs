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
-- See <https://gitlab.haskell.org/ghc/ghc/-/issues/19312>
--
-- Use lazy fields so that we can test values with missing fields.
--
-- Test both the case where the name of the type and the name of the constructor
-- are the same and where they are different.
largeRecord (defaultPureScript {allFieldsStrict = False}) [d|
    data R a = MkR { x :: Int, y :: [a] }
    data S a = S   { x :: Int, y :: [a] }
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

inOrder :: R Bool
inOrder = [mkRecord| MkR { x = 1234, y = [True] } |]

outOfOrder :: R Bool
outOfOrder = [mkRecord| MkR { y = [True], x = 1234 } |]

-- Results in "Unexpected fields" error
-- extraFields :: R
-- extraFields = [mkRecord| MkR { x = 1234, y = True, z = () } |]

-- But this works (with a warning)
missingFields :: R Bool
missingFields = [mkRecord| MkR { x = 1234 } |]

valueOfS :: S Bool
valueOfS = [mkRecord| S { x = 1234, y = [True] } |]

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
    assertEqual "R/S"                   inOrder.x valueOfS.x
