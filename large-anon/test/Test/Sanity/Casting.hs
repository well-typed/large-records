{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.Casting (tests) where

import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.Casting" [
      testCase "id"      test_id
    , testCase "reorder" test_reorder
    , testCase "merge"   test_merge
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record I '[ '("a", Bool), '("b", Char) ]
recordA =
      Anon.insert #a (I True)
    $ Anon.insert #b (I 'a')
    $ Anon.empty

recordA' :: Record I '[ '("b", Char), '("a", Bool) ]
recordA' =
      Anon.insert #b (I 'a')
    $ Anon.insert #a (I True)
    $ Anon.empty

recordWithMerge :: Record I (Anon.Merge '[ '("a", Bool) ] '[ '("b", Char) ])
recordWithMerge =
    Anon.merge
      (Anon.insert #a (I True) $ Anon.empty)
      (Anon.insert #b (I 'a')  $ Anon.empty)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_id :: Assertion
test_id = assertEqual "" recordA $ Anon.castRecord recordA

test_reorder :: Assertion
test_reorder = assertEqual "" recordA' $ Anon.castRecord recordA

test_merge :: Assertion
test_merge = assertEqual "" recordA $ Anon.castRecord recordWithMerge
