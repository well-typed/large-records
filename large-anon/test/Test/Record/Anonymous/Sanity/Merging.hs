{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.Merging (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.Merging" [
      testCase "concrete"    test_concrete
    , testCase "polymorphic" test_polymorphic
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

ab, ab' ::
    Record (Merge '[ '("a", Bool), '("b", Int)]
                  '[ '("c", Double), '("d", Char)])
ab  = merge a b
ab' = merge a' b

a, a' :: Record '[ '("a", Bool), '("b", Int)]
a =
    insert #a True
  $ insert #b (1 :: Int)
  $ empty
a' =
    insert #a False
  $ insert #b (1 :: Int)
  $ empty

b :: Record '[ '("c", Double), '("d", Char)]
b = insert #c 3.14
  $ insert #d 'a'
  $ empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_concrete :: Assertion
test_concrete = do
    assertEqual "get" True $ get #a ab
    assertEqual "set" ab'  $ set #a False ab

test_polymorphic :: Assertion
test_polymorphic = do
    assertEqual "get" 1   $ getPoly ab
    assertEqual "set" ab' $ setPoly ab
  where
    getPoly :: Record (Merge '[ '("a", Bool), '("b", Int)] r) -> Int
    getPoly = get #b

    setPoly ::
         Record (Merge '[ '("a", Bool), '("b", Int)] r)
      -> Record (Merge '[ '("a", Bool), '("b", Int)] r)
    setPoly = set #a False



