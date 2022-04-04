{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.RecordLens (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.RecordLens" [
      testGroup "Isomorphic projections" [
          testCase "id"      test_id
        , testCase "reorder" test_reorder
        , testCase "merge"   test_merge
        ]
    , testGroup "General lenses" [
          testCase "lens" test_lens
        ]
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record I [ "a" := Bool, "b" := Char, "c" := Int ]
recordA =
      Anon.insert #a (I True)
    $ Anon.insert #b (I 'a')
    $ Anon.insert #c (I 1)
    $ Anon.empty

recordA' :: Record I [ "b" := Char, "a" := Bool, "c" := Int ]
recordA' =
      Anon.insert #b (I 'a')
    $ Anon.insert #a (I True)
    $ Anon.insert #c (I 1)
    $ Anon.empty

recordWithMerge :: Record I (Merge '[ "a" := Bool ] [ "b" := Char, "c" := Int ])
recordWithMerge =
    Anon.merge
      ( Anon.insert #a (I True)
      $ Anon.empty
      )
      ( Anon.insert #b (I 'a')
      $ Anon.insert #c (I 1)
      $ Anon.empty
      )

recordB :: Record I [ "c" := Int, "b" := Char ]
recordB =
      Anon.insert #c (I 1)
    $ Anon.insert #b (I 'a')
    $ Anon.empty

{-------------------------------------------------------------------------------
  Tests for isomorphic projections
-------------------------------------------------------------------------------}

test_id :: Assertion
test_id = assertEqual "" recordA $ Anon.project recordA

test_reorder :: Assertion
test_reorder = assertEqual "" recordA' $ Anon.project recordA

test_merge :: Assertion
test_merge = assertEqual "" recordA $ Anon.project recordWithMerge

{-------------------------------------------------------------------------------
  Test for more general lenses
-------------------------------------------------------------------------------}

test_lens :: Assertion
test_lens = do
    let (getter, setter) = Anon.lens recordA
    assertEqual "get" recordB $
      getter
    assertEqual "set" (Anon.set #c (I 2) recordA) $
      setter (Anon.set #c (I 2) recordB)
