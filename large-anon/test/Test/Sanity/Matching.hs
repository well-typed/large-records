{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Matching (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Simple as S
import Data.Record.Anon.Advanced as A

tests :: TestTree
tests = testGroup "Test.Sanity.Matching" [
      testCase "simple"   test_simple
    , testCase "advanced" test_advanced
    ]

a :: S.Record ["a" := Int, "b" := String]
a = ANON{a = 1, b = "2"}

f1 :: S.Record ["a" := Int, "b" := String] -> String
f1 ANON{b = b', a = a'} = show a' ++ " " ++ b'

f2 :: S.Record ["a" := Int, "b" := String] -> String
f2 ANON{a = a'} = show a'

f3 :: S.Record ["a" := Int, "b" := String] -> String
f3 ANON{} = ""

f4 :: RowHasField "a" r Int => S.Record r -> String
f4 ANON{a = a'} = show a'

f5 :: S.Record ("b" := String : r) -> String
f5 ANON{b = b'} = b'

f6 :: A.Record I ["a" := Bool, "b" := Int] -> Either Int Int
f6 ANON_F{a = I True,  b = I b'} = Left b'
f6 ANON_F{a = I _,     b = I b'} = Right b'

test_simple :: Assertion
test_simple = do
    assertEqual "total"        "1 2" $ f1 a
    assertEqual "partial 1"    "1"   $ f2 a
    assertEqual "empty"        ""    $ f3 a
    assertEqual "constrainted" "1"   $ f4 a
    assertEqual "row"          "2"   $ f5 @'[] (S.project a)

test_advanced :: Assertion
test_advanced = do
  assertEqual "advenced 1"   (Left 42)  $ f6 ANON_F{a = I True,  b = I 42}
  assertEqual "advenced 2"   (Right 42) $ f6 ANON_F{a = I False, b = I 42}