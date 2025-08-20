{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- for the ToJSON/FromJSON instances

module Test.Sanity.Generics (tests) where

import Data.Aeson
import Data.Record.Generic

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

import Test.Infra.Generics

import qualified Test.Sanity.Named.Record1 as R1
import qualified Test.Sanity.Named.Record2 as R2

-- add test with non-I functor

tests :: TestTree
tests = testGroup "Test.Sanity.Generics" [
      testCase "Show"           test_Show
    , testCase "Eq"             test_Eq
    , testCase "Ord"            test_Ord
    , testCase "describeRecord" test_describeRecord
    , testCase "JSON"           test_JSON
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

type TypeRecord1 = Record I [ "x" := Bool, "y" := Char, "z" := () ]

record1 :: Record I [ "x" := Bool, "y" := Char, "z" := () ]
record1 =
      Anon.insert #x (I True)
    $ Anon.insert #y (I 'a')
    $ Anon.insert #z (I ())
    $ Anon.empty

-- | Example where the fields do not appear in alphabetical order
--
-- Ordering matters in the 'Generic' instance.
record2 :: Record I [ "y" := Char, "x" := Bool ]
record2 =
      Anon.insert #y (I 'a')
    $ Anon.insert #x (I True)
    $ Anon.empty

-- | Example that doesn't use I as the functor
record3 :: Record (K ()) [ "y" := Char, "x" := Bool ]
record3 =
      Anon.insert #y (K ())
    $ Anon.insert #x (K ())
    $ Anon.empty

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_Show :: Assertion
test_Show = do
    assertEqual "R1" (show (R1.ANON_F (I True) (I 'a') (I ()))) $ show record1
    assertEqual "R2" (show (R2.ANON_F (I 'a')  (I True)))       $ show record2
    assertEqual "R3" (show (R2.ANON_F (K ())   (K ())))         $ show record3

test_Eq :: Assertion
test_Eq = do
    assertEqual "equal" True $
      record1 == record1
    assertEqual "not equal" False $
      record1 == (Anon.set #x (I False) record1)

test_Ord :: Assertion
test_Ord = do
    assertEqual "R1" (compare (R1.ANON_F (I True) (I 'a') (I ())) (R1.ANON_F (I False) (I 'a') (I ()))) $
      compare record1 (Anon.set #x (I False) record1)
    assertEqual "R2" (compare (R2.ANON_F (I 'a') (I True)) (R2.ANON_F (I 'a') (I False))) $
      compare record2 (Anon.set #x (I False) record2)

-- Test 'describeRecord'
--
-- The primary motivation for 'test_describeRecord' is actually not to test the
-- function itself, but to verify that constraint resolution is working ok.
-- Specifically, that the implicit kind argument to 'Typeable' is handled by ghc
-- and does not need to be taken into account by the @large-anon@ plugin.
test_describeRecord :: Assertion
test_describeRecord = do
    assertEqual "" expected $ describeRecord (Proxy @TypeRecord1)
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
