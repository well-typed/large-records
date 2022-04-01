{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.CheckCanProject (tests) where

import Data.Typeable

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)

import Test.Tasty
import Test.Tasty.HUnit

import Test.Infra.Discovery

tests :: TestTree
tests = testGroup "Test.Sanity.CheckCanProject" [
      testCase "checkCanProject" test_checkCanProject
    ]

test_checkCanProject :: Assertion
test_checkCanProject = do
    case maybeProject example1 (Proxy @Row2) of
      Left  _  -> assertFailure "Should be able to project to Row2"
      Right r1 -> assertEqual "Should be equal" r1 example2

    case maybeProject example1 (Proxy @Row3) of
      Left missing -> assertEqual "missing" ["c"] missing
      Right _      -> assertFailure "Should not be able to project to Row3"

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

type Row1 = [ "a" := Int, "b" := Bool, "c" := Char ]
type Row2 = [ "c" := Char, "a" := Int ]
type Row3 = [ "c" := Bool, "a" := Int ]

example1 :: Record I Row1
example1 = ANON_F {
      a = I 1
    , b = I True
    , c = I 'a'
    }

example2 :: Record I Row2
example2 = ANON_F {
      c = I 'a'
    , a = I 1
    }
