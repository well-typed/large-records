{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.Casting (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.Casting" [
      testCase "id"      test_id
    , testCase "reorder" test_reorder
    , testCase "merge"   test_merge
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record I '[ '("a", Bool), '("b", Char) ]
recordA =
      insert #a (I True)
    $ insert #b (I 'a')
    $ empty

recordA' :: Record I '[ '("b", Char), '("a", Bool) ]
recordA' =
      insert #b (I 'a')
    $ insert #a (I True)
    $ empty

recordWithMerge :: Record I (Merge '[ '("a", Bool) ] '[ '("b", Char) ])
recordWithMerge =
    merge
      (insert #a (I True) $ empty)
      (insert #b (I 'a')  $ empty)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_id :: Assertion
test_id = assertEqual "" recordA $ castRecord recordA

test_reorder :: Assertion
test_reorder = assertEqual "" recordA' $ castRecord recordA

test_merge :: Assertion
test_merge = assertEqual "" recordA $ castRecord recordWithMerge
