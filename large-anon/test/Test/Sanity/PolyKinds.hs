{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Test.Sanity.PolyKinds (tests) where

import Data.Record.Anonymous.Advanced (Record, Pair((:=)))
import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

import Test.Infra.MarkStrictness

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.PolyKinds" [
      testCase "hasField" test_hasField
    , testCase "show"     test_show
    , testCase "project"  test_project
    , testCase "merge"    test_merge
    ]

-- | Test generics ('AllFields' and 'KnownFields')
test_show :: Assertion
test_show =
    assertEqual "" expected $
      show exampleRecord1'
  where
    expected :: String
    expected = "Record {a = True, b = 1234}"

-- | 'HasField' (and 'KnownHash', but that's no different for polykinds)
test_hasField :: Assertion
test_hasField = do
    assertEqual "get" (BoxStrict 1234) $
      Anon.get #b $ exampleRecord1
    assertEqual "set" exampleRecord1' $
      Anon.set #a (BoxLazy True) $ exampleRecord1

-- | 'Project'
--
-- NOTE: The projection must ignore the undefined value.
test_project :: Assertion
test_project =
    assertEqual "" exampleRecord2 $
      Anon.project exampleRecord1

-- | Merging
test_merge :: Assertion
test_merge =
    assertEqual "" exampleRecord1' $
      Anon.project $ Anon.merge exampleRecord2 exampleRecord3

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

exampleRecord1, exampleRecord1' ::
  Record Boxed [ "a" := Lazy   Bool
               , "b" := Strict Int
               ]
exampleRecord1 =
      Anon.insert #a (BoxLazy undefined)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty
exampleRecord1' =
      Anon.insert #a (BoxLazy True)
    $ Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

exampleRecord2 :: Record Boxed '[ "b" := Strict Int ]
exampleRecord2 =
      Anon.insert #b (BoxStrict 1234)
    $ Anon.empty

exampleRecord3 :: Record Boxed '[ "a" := Lazy Bool ]
exampleRecord3 =
      Anon.insert #a (BoxLazy True)
    $ Anon.empty
