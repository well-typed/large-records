{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.Lens (tests) where

import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

-- TODO: We should have a Data.Record.Anonymous module for unqualified imports
import Data.Record.Anonymous.Advanced (Record, Pair((:=)))
import qualified Data.Record.Anonymous.Advanced as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.Lens" [
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

recordWithMerge :: Record I (Anon.Merge '[ "a" := Bool ] [ "b" := Char, "c" := Int ])
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
