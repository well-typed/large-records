{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=TypeLet -fplugin=Data.Record.Anonymous.Plugin #-}
-- {-# OPTIONS_GHC -fplugin-opt=Data.Record.Anonymous.Plugin:debug   #-}
{-# OPTIONS_GHC -fplugin-opt=Data.Record.Anonymous.Plugin:typelet #-}

-- | Tests for @typelet@
--
-- These tests are identical to the ones in @WithoutTypelet@, except that we
-- enable the @typelet@ plugin option here.
module Test.Sanity.SrcPlugin.WithTypelet (tests) where

import Data.SOP.BasicFunctors

import Data.Record.Anonymous.Advanced (Pair((:=)))

import qualified Data.Record.Anonymous.Advanced as A
import qualified Data.Record.Anonymous.Simple   as S

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.SrcPlugin.WithTypelet" [
      testCase "simple"   test_simple
    , testCase "advanced" test_advanced
    ]

test_simple :: Assertion
test_simple = do
    assertEqual "inorder"   expected $ example_simple
    assertEqual "reordered" expected $ example_simple_reordered
  where
    expected :: S.Record '[ "a" := Int, "b" := Char, "c" := Bool ]
    expected =
          S.insert #a 1
        $ S.insert #b 'a'
        $ S.insert #c True
        $ S.empty

test_advanced :: Assertion
test_advanced = do
    assertEqual "I"     expectedI     $ example_advanced_I
    assertEqual "Maybe" expectedMaybe $ example_advanced_Maybe
  where
    expectedI :: A.Record I [ "a" := Int, "b" := Char, "c" := Bool]
    expectedI =
          A.insert #a (I 1)
        $ A.insert #b (I 'a')
        $ A.insert #c (I True)
        $ A.empty

    expectedMaybe :: Maybe (A.Record I '[ "b" := Char, "a" := Bool ])
    expectedMaybe = Just $
          A.insert #b (I 'a')
        $ A.insert #a (I True)
        $ A.empty

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

example_simple :: S.Record '[ "a" := Int, "b" := Char, "c" := Bool ]
example_simple = ANON {
      a = 1
    , b = 'a'
    , c = True
    }

-- | Check that the use of typelet does not hinder type inference
example_simple_reordered :: S.Record '[ "a" := Int, "b" := Char, "c" := Bool ]
example_simple_reordered = S.project $ ANON {
      a = 1
    , c = True
    , b = 'a'
    }

example_advanced_I :: A.Record I '[ "a" := Int, "b" := Char, "c" := Bool ]
example_advanced_I = ANON_F {
      a = I 1
    , b = I 'a'
    , c = I True
    }

example_advanced_Maybe :: Maybe (A.Record I '[ "b" := Char, "a" := Bool ])
example_advanced_Maybe = A.sequenceA' $ ANON_F {
      b = Just 'a'
    , a = Just True
    }
