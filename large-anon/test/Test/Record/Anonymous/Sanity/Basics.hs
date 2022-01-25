{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- for the ToJSON/FromJSON instances

module Test.Record.Anonymous.Sanity.Basics (tests) where

import Data.Aeson
import Data.SOP.BasicFunctors -- TODO: Should this be exported from large-anon?

import qualified Data.Record.Anonymous as A

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Record.Anonymous.Sanity.Named.Record1 as R1
import qualified Test.Record.Anonymous.Sanity.Named.Record2 as R2

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.Basics" [
      testCase "HasField"       test_HasField
    , testCase "Show"           test_Show
    , testCase "Eq"             test_Eq
    , testCase "Ord"            test_Ord
    , testCase "describeRecord" test_describeRecord
    , testCase "JSON"           test_JSON
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

record1 :: A.Record I '[ '("x", Bool), '("y", Char), '("z", ()) ]
record1 =
      A.insert #x (I True)
    $ A.insert #y (I 'a')
    $ A.insert #z (I ())
    $ A.empty

-- | Second example, where the fields do not appear in alphabetical order
--
-- Ordering matters in the 'Generic' instance.
record2 :: A.Record I '[ '("y", Char), '("x", Bool) ]
record2 =
      A.insert #y (I 'a')
    $ A.insert #x (I True)
    $ A.empty

test_HasField :: Assertion
test_HasField = do
    assertEqual "get field 1" (I True) $ (A.get #x record1)
    assertEqual "get field 2" (I 'a')  $ (A.get #y record1)
    assertEqual "get field 3" (I ())   $ (A.get #z record1)

    -- TODO: We should do whole-record comparisons, but for that we need
    -- Show and Eq instances, which will depend on generics

    assertEqual "set field 1, then get field 1" (I False) $
      A.get #x (A.set #x (I False) record1)
    assertEqual "set field 1, then get field 2" (I 'a') $
      (A.get #y (A.set #x (I False) record1))

    -- TODO: think about and test what happens with duplicate labels

test_Show :: Assertion
test_Show = do
    assertEqual "R1" (show (R1.Record (I True) (I 'a') (I ()))) $ show record1
    assertEqual "R2" (show (R2.Record (I 'a') (I True)))        $ show record2

test_Eq :: Assertion
test_Eq = do
    assertEqual "equal" True $
      record1 == record1
    assertEqual "not equal" False $
      record1 == (A.set #x (I False) record1)

test_Ord :: Assertion
test_Ord = do
    assertEqual "R1" (compare (R1.Record (I True) (I 'a') (I ())) (R1.Record (I False) (I 'a') (I ()))) $
      compare record1 (A.set #x (I False) record1)
    assertEqual "R2" (compare (R2.Record (I 'a') (I True)) (R2.Record (I 'a') (I False))) $
      compare record2 (A.set #x (I False) record2)

-- Test 'describeRecord'
--
-- The primary motivation for this test is actually not the function itself,
-- but to verify that constraint resolution is working ok. Specifically,
-- that the implicit kind argument to 'Typeable' is handled by ghc and does not
-- need to be taken into account by the @large-anon@ plugin.
test_describeRecord :: Assertion
test_describeRecord = do
    assertEqual "" expected $ A.describeRecord record1
  where
    expected :: String
    expected = "Record {x :: I Bool, y :: I Char, z :: I ()}"

test_JSON :: Assertion
test_JSON = do
    assertEqual "R1" (Just record1) $ decode (encode record1)
    assertEqual "R2" (Just record2) $ decode (encode record2)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

instance FromJSON a => FromJSON (I a) where
  parseJSON = fmap I . parseJSON

instance ToJSON a => ToJSON (I a) where
  toJSON = toJSON . unI