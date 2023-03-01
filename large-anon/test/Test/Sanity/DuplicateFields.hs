{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.DuplicateFields (tests) where

import Data.Proxy
import Data.Record.Generic.LowerBound
import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Test.Infra.Generics

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
    , testGroup "Lenses" [
          testCase "project" test_project
        , testCase "update"  test_update
        ]
    , testGroup "Merging" [
          testCase "mergeSameType"       test_mergeSameType
        , testCase "mergeDifferentType"  test_mergeDifferentType
        ]
    ]

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

type InterspersedSameType =
       Record I [ "a" := Char
                , "b" := Word
                , "c" := ()
                , "b" := Word
                , "d" := [Double]
                ]

type InterspersedDiffType =
       Record I [ "a" := Char
                , "b" := Word
                , "c" := ()
                , "b" := Bool
                , "d" := [Double]
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
      describeRecord (Proxy @InterspersedSameType)
    assertEqual "diff" expectedDiff $
      describeRecord (Proxy @InterspersedDiffType)
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
          "ANON_F {a = I 'a'"
        ,       ", b = I 1"
        ,       ", c = I ()"
        ,       ", b = I 2"
        ,       ", d = I [3.14]"
        ,         "}"
        ]
    expectedDiff = concat [
          "ANON_F {a = I 'a'"
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
  Lenses
-------------------------------------------------------------------------------}

test_project :: Assertion
test_project = do
    assertEqual "same" expected $
      Anon.project interspersedSameType
    assertEqual "diff" expected $
      Anon.project interspersedDiffType
  where
    expected :: Record I [ "a" := Char
                         , "b" := Word
                         , "c" := ()
                         , "d" := [Double]
                         ]
    expected =
          Anon.insert #a (I 'a')
        $ Anon.insert #b (I 1)
        $ Anon.insert #c (I ())
        $ Anon.insert #d (I [3.14])
        $ Anon.empty

test_update :: Assertion
test_update = do
    assertEqual "same" (upd interspersedSameType) $
      setSame new
    assertEqual "diff" (upd interspersedDiffType) $
      setDiff new
  where
    (_, setSame) = Anon.lens interspersedSameType
    (_, setDiff) = Anon.lens interspersedDiffType

    upd :: RowHasField "d" r [Double] => Record I r -> Record I r
    upd r = Anon.set #d (I [1.618]) r

    new :: Record I '[ "d" := [Double] ]
    new = Anon.insert #d (I [1.618]) $ Anon.empty

{-------------------------------------------------------------------------------
  Merging
-------------------------------------------------------------------------------}

test_mergeSameType :: Assertion
test_mergeSameType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ "a" := Bool ]
    actual = Anon.project $
               Anon.merge
                 (Anon.insert #a (I True)  Anon.empty)
                 (Anon.insert #a (I False) Anon.empty)

    expected :: Record I '[ "a" := Bool ]
    expected = Anon.insert #a (I True) Anon.empty

test_mergeDifferentType :: Assertion
test_mergeDifferentType = do
    assertEqual "" expected actual
  where
    actual :: Record I '[ "a" := Bool ]
    actual = Anon.project $
               Anon.merge (Anon.insert #a (I True) Anon.empty)
                          (Anon.insert #a (I 'a')  Anon.empty)

    expected :: Record I '[ "a" := Bool ]
    expected = Anon.insert #a (I True) Anon.empty
