{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

-- | Tests for the @Simple@ interface
--
-- The @Simple@ is a thin wrapper around the @Advanced@ interface, so a few
-- select sanity tests suffice.
module Test.Sanity.Simple (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anonymous.Simple (Record)
import qualified Data.Record.Anonymous.Simple as Anon

tests :: TestTree
tests = testGroup "Test.Sanity.Simple" [
      testCase "hasField" test_hasField
    , testCase "merge"    test_merge
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record '[ '("a", Bool), '("b", Char) ]
recordA =
      Anon.insert #a True
    $ Anon.insert #b 'a'
    $ Anon.empty

recordA' :: Record '[ '("a", Bool), '("b", Char) ]
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
    assertEqual "left"  recordA $ Anon.castRecord $ Anon.merge Anon.empty recordA
    assertEqual "right" recordA $ Anon.castRecord $ Anon.merge recordA Anon.empty