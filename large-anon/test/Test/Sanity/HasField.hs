{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.HasField (tests) where

import Data.SOP.BasicFunctors -- TODO: Should this be exported from large-anon?

import Data.Record.Anonymous.Advanced (Record, Pair((:=)))
import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Sanity.HasField" [
      testCase "HasField" test_HasField
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

record1 :: Record I [ "x" := Bool, "y" := Char, "z" := () ]
record1 =
      Anon.insert #x (I True)
    $ Anon.insert #y (I 'a')
    $ Anon.insert #z (I ())
    $ Anon.empty

record1' :: Record I [ "x" := Bool, "y" := Char, "z" := () ]
record1' =
      Anon.insert #x (I False)
    $ Anon.insert #y (I 'a')
    $ Anon.insert #z (I ())
    $ Anon.empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_HasField :: Assertion
test_HasField = do
    -- get

    assertEqual "get field 1" (I True) $ Anon.get #x record1
    assertEqual "get field 2" (I 'a')  $ Anon.get #y record1
    assertEqual "get field 3" (I ())   $ Anon.get #z record1

    -- set then get

    assertEqual "set field 1, then get field 1" (I False) $
      Anon.get #x (Anon.set #x (I False) record1)
    assertEqual "set field 1, then get field 2" (I 'a') $
      Anon.get #y (Anon.set #x (I False) record1)

    -- set

    assertEqual "set field 1" record1' $ Anon.set #x (I False) record1


