{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.Merging (tests) where

import Data.SOP.BasicFunctors

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.Merging" [
      testCase "concrete"     test_concrete
    , testCase "polymorphic"  test_polymorphic
    , testCase "eqConstraint" test_eqConstraint
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

ab, ab' ::
    Record I (Anon.Merge '[ '("a", Bool), '("b", Int)]
                         '[ '("c", Double), '("d", Char)])
ab  = Anon.merge a b
ab' = Anon.merge a' b

a, a' :: Record I '[ '("a", Bool), '("b", Int)]
a =
    Anon.insert #a (I True)
  $ Anon.insert #b (I (1 :: Int))
  $ Anon.empty
a' =
    Anon.insert #a (I False)
  $ Anon.insert #b (I (1 :: Int))
  $ Anon.empty

b :: Record I '[ '("c", Double), '("d", Char)]
b = Anon.insert #c (I 3.14)
  $ Anon.insert #d (I 'a')
  $ Anon.empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_concrete :: Assertion
test_concrete = do
    assertEqual "get" (I True) $ Anon.get #a ab
    assertEqual "set" ab'      $ Anon.set #a (I False) ab

test_polymorphic :: Assertion
test_polymorphic = do
    assertEqual "get" (I 1) $ getPoly ab
    assertEqual "set" ab'   $ setPoly ab
  where
    getPoly :: Record I (Anon.Merge '[ '("a", Bool), '("b", Int)] r) -> I Int
    getPoly = Anon.get #b

    setPoly ::
         Record I (Anon.Merge '[ '("a", Bool), '("b", Int)] r)
      -> Record I (Anon.Merge '[ '("a", Bool), '("b", Int)] r)
    setPoly = Anon.set #a (I False)

-- | Test that type equalities are handled correctly
test_eqConstraint :: Assertion
test_eqConstraint = do
    assertEqual "a" (I True) $ f1 ab
    assertEqual "b" (I 1)    $ f2 ab
    assertEqual "c" (I 3.14) $ f3 ab
  where
    -- Single simple equality
    f1 :: row ~ Anon.Merge '[ '("a", Bool), '("b", Int)]
                           '[ '("c", Double), '("d", Char)]
       => Record I row -> I Bool
    f1 = Anon.get #a

    -- Multiple (transitive) equalities
    f2 :: ( tf1 ~ tf2
          , tf2 ~ Anon.Merge
          , row ~ tf1 '[ '("a", Bool), '("b", Int)]
                      '[ '("c", Double), '("d", Char)]
          )
       => Record I row -> I Int
    f2 = Anon.get #b

    -- Equality with partial application
    f3 :: ( merge ~ Anon.Merge '[ '("a", Bool), '("b", Int)]
          , row   ~ merge      '[ '("c", Double), '("d", Char)]
          )
       => Record I row -> I Double
    f3 = Anon.get #c
