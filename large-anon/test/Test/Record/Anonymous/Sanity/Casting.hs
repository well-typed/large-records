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

recordA :: Record '[ '("a", Bool), '("b", Char) ]
recordA =
      insert #a True
    $ insert #b 'a'
    $ empty

recordA' :: Record '[ '("b", Char), '("a", Bool) ]
recordA' =
      insert #b 'a'
    $ insert #a True
    $ empty

recordWithMerge :: Record (Merge '[ '("a", Bool) ] '[ '("b", Char) ])
recordWithMerge =
    merge
      (insert #a True $ empty)
      (insert #b 'a'  $ empty)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_id :: Assertion
test_id = assertEqual "" recordA $ castRecord recordA

test_reorder :: Assertion
test_reorder = assertEqual "" recordA' $ castRecord recordA

test_merge :: Assertion
test_merge = assertEqual "" recordA $ castRecord recordWithMerge