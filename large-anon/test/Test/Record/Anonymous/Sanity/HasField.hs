{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Record.Anonymous.Sanity.HasField (tests) where

import Data.SOP.BasicFunctors -- TODO: Should this be exported from large-anon?

import Data.Record.Anonymous.Advanced (Record)
import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.HasField" [
      testCase "HasField" test_HasField
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

record1 :: Record I '[ '("x", Bool), '("y", Char), '("z", ()) ]
record1 =
      Anon.insert #x (I True)
    $ Anon.insert #y (I 'a')
    $ Anon.insert #z (I ())
    $ Anon.empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_HasField :: Assertion
test_HasField = do
    assertEqual "get field 1" (I True) $ (Anon.get #x record1)
    assertEqual "get field 2" (I 'a')  $ (Anon.get #y record1)
    assertEqual "get field 3" (I ())   $ (Anon.get #z record1)

    -- TODO: We should do whole-record comparisons, but for that we need
    -- Show and Eq instances, which will depend on generics

    assertEqual "set field 1, then get field 1" (I False) $
      Anon.get #x (Anon.set #x (I False) record1)
    assertEqual "set field 1, then get field 2" (I 'a') $
      (Anon.get #y (Anon.set #x (I False) record1))

    -- TODO: think about and test what happens with duplicate labels
