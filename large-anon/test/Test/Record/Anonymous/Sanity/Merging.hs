{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.Merging (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.Merging" [
      testCase "concrete"     test_concrete
    , testCase "polymorphic"  test_polymorphic
    , testCase "eqConstraint" test_eqConstraint
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

ab, ab' ::
    Record I (Merge '[ '("a", Bool), '("b", Int)]
                    '[ '("c", Double), '("d", Char)])
ab  = merge a b
ab' = merge a' b

a, a' :: Record I '[ '("a", Bool), '("b", Int)]
a =
    insert #a (I True)
  $ insert #b (I (1 :: Int))
  $ empty
a' =
    insert #a (I False)
  $ insert #b (I (1 :: Int))
  $ empty

b :: Record I '[ '("c", Double), '("d", Char)]
b = insert #c (I 3.14)
  $ insert #d (I 'a')
  $ empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_concrete :: Assertion
test_concrete = do
    assertEqual "get" (I True) $ get #a ab
    assertEqual "set" ab'      $ set #a (I False) ab

test_polymorphic :: Assertion
test_polymorphic = do
    assertEqual "get" (I 1) $ getPoly ab
    assertEqual "set" ab'   $ setPoly ab
  where
    getPoly :: Record I (Merge '[ '("a", Bool), '("b", Int)] r) -> I Int
    getPoly = get #b

    setPoly ::
         Record I (Merge '[ '("a", Bool), '("b", Int)] r)
      -> Record I (Merge '[ '("a", Bool), '("b", Int)] r)
    setPoly = set #a (I False)

-- | Test that type equalities are handled correctly
test_eqConstraint :: Assertion
test_eqConstraint = do
    assertEqual "a" (I True) $ f1 ab
    assertEqual "b" (I 1)    $ f2 ab
    assertEqual "c" (I 3.14) $ f3 ab
  where
    -- Single simple equality
    f1 :: row ~ Merge '[ '("a", Bool), '("b", Int)]
                      '[ '("c", Double), '("d", Char)]
       => Record I row -> I Bool
    f1 = get #a

    -- Multiple (transitive) equalities
    f2 :: ( tf1 ~ tf2
          , tf2 ~ Merge
          , row ~ tf1 '[ '("a", Bool), '("b", Int)]
                      '[ '("c", Double), '("d", Char)]
          )
       => Record I row -> I Int
    f2 = get #b

    -- Equality with partial application
    f3 :: ( merge ~ Merge '[ '("a", Bool), '("b", Int)]
          , row   ~ merge '[ '("c", Double), '("d", Char)]
          )
       => Record I row -> I Double
    f3 = get #c
