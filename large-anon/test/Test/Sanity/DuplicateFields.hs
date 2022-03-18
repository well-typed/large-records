{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.DuplicateFields (tests) where

import Data.Proxy
import Data.Record.Generic.LowerBound
import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon
import Data.Record.Anonymous.Internal.Generics (debugFieldTypes)

-- TODO: More tests, cleanup. Then remove comments about testing duplicate
-- fields elsewhere in the test suite.
--
-- TODO: Add support for /removing/ fields, add tests for that. Then we'll
-- truly have scoping. I think it'll be easy now.

tests :: TestTree
tests = testGroup "Test.Sanity.DuplicateFields" [
      testGroup "Generics" [
          testCase "fieldTypes" test_fieldTypes
        , testCase "describe"   test_describe
        , testCase "show"       test_show
        , testCase "read"       test_read
        ]
    , testGroup "HasField" [
          testCase "get" test_get
        , testCase "set" test_set
        ]
    , testGroup "OLD -- TODO: Tidy up" [
          testCase "insertSameType"      test_insertSameType
        , testCase "insertDifferentType" test_insertDifferentType
        , testCase "mergeSameType"       test_mergeSameType
        , testCase "mergeDifferentType"  test_mergeDifferentType
        ]
    ]

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

type InterspersedSameType =
       Record I '[ '("a", Char)
                 , '("b", Word)
                 , '("c", ())
                 , '("b", Word)
                 , '("d", [Double])
                 ]

type InterspersedDiffType =
       Record I '[ '("a", Char)
                 , '("b", Word)
                 , '("c", ())
                 , '("b", Bool)
                 , '("d", [Double])
                 ]

interspersedSameType :: InterspersedSameType
interspersedSameType =
      Anon.insert #a (I 'a')
    $ Anon.insert #b (I 1)
    $ Anon.insert #c (I ())
    $ Anon.insert #b (I 2)
    $ Anon.insert #d (I [3.14])
    $ Anon.empty

interspersedDiffType :: InterspersedDiffType
interspersedDiffType =
      Anon.insert #a (I 'a')
    $ Anon.insert #b (I 1)
    $ Anon.insert #c (I ())
    $ Anon.insert #b (I True)
    $ Anon.insert #d (I [3.14])
    $ Anon.empty

{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

test_fieldTypes :: Assertion
test_fieldTypes = do
    assertEqual "same" expected $
      debugFieldTypes (Proxy @InterspersedSameType)
    assertEqual "diff" expected $
      debugFieldTypes (Proxy @InterspersedDiffType)
  where
    expected :: String
    expected = "[a,b,c,b,d]"

test_describe :: Assertion
test_describe = do
    assertEqual "same" expectedSame $
      Anon.describeRecord (Proxy @InterspersedSameType)
    assertEqual "diff" expectedDiff $
      Anon.describeRecord (Proxy @InterspersedDiffType)
  where
    expectedSame, expectedDiff :: String
    expectedSame = concat [
        "Record {a :: I Char"
      ,       ", b :: I Word"
      ,       ", c :: I ()"
      ,       ", b :: I Word"
      ,       ", d :: I [Double]"
      ,        "}"
      ]
    expectedDiff = concat [
        "Record {a :: I Char"
      ,       ", b :: I Word"
      ,       ", c :: I ()"
      ,       ", b :: I Bool"
      ,       ", d :: I [Double]"
      ,        "}"
      ]

test_show :: Assertion
test_show = do
    assertEqual "same" expectedSame $
      show interspersedSameType
    assertEqual "diff" expectedDiff $
      show interspersedDiffType
  where
    expectedSame, expectedDiff :: String
    expectedSame = concat [
          "Record {a = I 'a'"
        ,       ", b = I 1"
        ,       ", c = I ()"
        ,       ", b = I 2"
        ,       ", d = I [3.14]"
        ,         "}"
        ]
    expectedDiff = concat [
          "Record {a = I 'a'"
        ,       ", b = I 1"
        ,       ", c = I ()"
        ,       ", b = I True"
        ,       ", d = I [3.14]"
        ,         "}"
        ]

test_read :: Assertion
test_read = do
    assertEqual "" expectedSame $
      Anon.cpure (Proxy @LowerBound) (I lowerBound)
    assertEqual "" expectedDiff $
      Anon.cpure (Proxy @LowerBound) (I lowerBound)
  where
    expectedSame :: InterspersedSameType
    expectedSame =
          Anon.insert #a (I '\NUL')
        $ Anon.insert #b (I 0)
        $ Anon.insert #c (I ())
        $ Anon.insert #b (I 0)
        $ Anon.insert #d (I [])
        $ Anon.empty

    expectedDiff :: InterspersedDiffType
    expectedDiff =
          Anon.insert #a (I '\NUL')
        $ Anon.insert #b (I 0)
        $ Anon.insert #c (I ())
        $ Anon.insert #b (I False)
        $ Anon.insert #d (I [])
        $ Anon.empty

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

test_get :: Assertion
test_get = do
    assertEqual "same" (I 1) $
      Anon.get #b interspersedSameType
    assertEqual "diff" (I 1) $
      Anon.get #b interspersedDiffType

test_set :: Assertion
test_set = do
    assertEqual "same" expectedSameType $
      Anon.set #b (I 3) interspersedSameType
    assertEqual "diff" expectedDiffType $
      Anon.set #b (I 3) interspersedDiffType
  where
    expectedSameType :: InterspersedSameType
    expectedSameType =
          Anon.insert #a (I 'a')
        $ Anon.insert #b (I 3)
        $ Anon.insert #c (I ())
        $ Anon.insert #b (I 2)
        $ Anon.insert #d (I [3.14])
        $ Anon.empty

    expectedDiffType :: InterspersedDiffType
    expectedDiffType =
          Anon.insert #a (I 'a')
        $ Anon.insert #b (I 3)
        $ Anon.insert #c (I ())
        $ Anon.insert #b (I True)
        $ Anon.insert #d (I [3.14])
        $ Anon.empty

{-------------------------------------------------------------------------------
  Casting

  TODO: We should have examples where duplicate fields are mixed in with others.
  TODO: We should have examples of casting /to/ records wit duplicate fields
  (or make it explicitly part of the contract that this is not possible)
-------------------------------------------------------------------------------}

test_insertSameType :: Assertion
test_insertSameType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
                 Anon.insert #a (I True)
               $ Anon.insert #a (I False)
               $ Anon.empty

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_insertDifferentType :: Assertion
test_insertDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
                 Anon.insert #a (I True)
               $ Anon.insert #a (I 'a')
               $ Anon.empty

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_mergeSameType :: Assertion
test_mergeSameType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
               Anon.merge
                 (Anon.insert #a (I True)  Anon.empty)
                 (Anon.insert #a (I False) Anon.empty)

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty

test_mergeDifferentType :: Assertion
test_mergeDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ '("a", Bool) ]
    actual = Anon.castRecord $
               Anon.merge (Anon.insert #a (I True) Anon.empty)
                          (Anon.insert #a (I 'a')  Anon.empty)

    expected :: Record I '[ '("a", Bool) ]
    expected = Anon.insert #a (I True) Anon.empty
