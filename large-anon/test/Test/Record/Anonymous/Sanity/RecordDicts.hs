{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
{-# LANGUAGE TypeOperators #-}

module Test.Record.Anonymous.Sanity.RecordDicts (tests) where

import Data.Record.Anonymous

import qualified Data.Record.Anonymous as Anon

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Sanity.RecordDicts" [
      testCase "manual"  test_manual
    , testCase "derived" test_derived
    ]

{-------------------------------------------------------------------------------
  Example value
-------------------------------------------------------------------------------}

recordA :: Record I '[ '("a", Int), '("b", Bool), '("c", Char) ]
recordA =
      insert #a (I 1)
    $ insert #b (I True)
    $ insert #c (I 'a')
    $ empty

-- | Manually created record of dictionaries
--
-- Normally this record would be constructed by the plugin (for 'RecordDicts'
-- instance).
recordD :: Record (Dict Show :.: I) '[ '("a", Int), '("b", Bool), '("c", Char) ]
recordD =
      insert #a (Comp Dict)
    $ insert #b (Comp Dict)
    $ insert #c (Comp Dict)
    $ empty

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

showFields :: Record (Dict Show :.: f) r -> Record f r -> [String]
showFields ds xs = Anon.collapse $ Anon.zipWith aux ds xs
  where
    aux :: (Dict Show :.: f) x -> f x -> K String x
    aux (Comp Dict) x = K (show x)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

-- | Test with manually constructed record-of-dictionaries
--
-- Just a sanity check on the sanity check that the test makes sense.
test_manual :: Assertion
test_manual = do
     assertEqual "" expected $ showFields recordD recordA
  where
    expected :: [String]
    expected = ["I 1", "I True", "I 'a'"]

test_derived :: Assertion
test_derived = do
     assertEqual "" expected $ showFields (recordDictsF (Proxy @Show)) recordA
  where
    expected :: [String]
    expected = ["I 1", "I True", "I 'a'"]
