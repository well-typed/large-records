{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

-- | Tests for the @Simple@ interface
--
-- The @Simple@ is a thin wrapper around the @Advanced@ interface, so a few
-- select sanity tests suffice.
module Test.Sanity.Simple (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Simple (Record)

import qualified Data.Record.Anon.Simple as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.Simple" [
      testCase "hasField" test_hasField
    , testCase "merge"    test_merge
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record [ "a" := Bool, "b" := Char ]
recordA =
      Anon.insert #a True
    $ Anon.insert #b 'a'
    $ Anon.empty

recordA' :: Record [ "a" := Bool, "b" := Char ]
recordA' =
      Anon.insert #a False
    $ Anon.insert #b 'a'
    $ Anon.empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_hasField :: Assertion
test_hasField = do
    assertEqual "get" True     $ Anon.get #a recordA
    assertEqual "set" recordA' $ Anon.set #a False recordA

test_merge :: Assertion
test_merge = do
    assertEqual "left"  recordA $ Anon.project $ Anon.merge Anon.empty recordA
    assertEqual "right" recordA $ Anon.project $ Anon.merge recordA Anon.empty
