{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.AllFields (tests) where

import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Dict

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Test.Sanity.AllFields" [
      testCase "manual"  test_manual
    , testCase "derived" test_derived
    ]

{-------------------------------------------------------------------------------
  Example value
-------------------------------------------------------------------------------}

recordA :: Record I [ "a" := Int, "b" := Bool, "c" := Char ]
recordA =
      Anon.insert #a (I 1)
    $ Anon.insert #b (I True)
    $ Anon.insert #c (I 'a')
    $ Anon.empty

-- | Manually created record of dictionaries
--
-- Normally this record would be constructed by the plugin (for 'RecordDicts'
-- instance).
recordD :: Record (Dict Show :.: I) [ "a" := Int, "b" := Bool, "c" := Char ]
recordD =
      Anon.insert #a (Comp Dict)
    $ Anon.insert #b (Comp Dict)
    $ Anon.insert #c (Comp Dict)
    $ Anon.empty

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
     assertEqual "" expected $
       showFields (Anon.reifyAllFields (Proxy @Show)) recordA
  where
    expected :: [String]
    expected = ["I 1", "I True", "I 'a'"]
