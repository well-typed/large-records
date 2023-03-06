{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ < 902

module Test.Record.Sanity.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testCaseInfo "Test.Record.Sanity.OverloadedRecordUpdate" $
      return "Skipped for ghc < 9.2"

#else

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.OverloadedRecordUpdate (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic (Rep)
import Data.Record.Generic.Lens.VL
import Data.Record.Overloading
import Data.Record.Plugin

tests :: TestTree
tests = testGroup "Test.Record.Sanity.OverloadedRecordUpdate" [
      testGroup "get" [
          testCase "simple" test_get_simple
        , testCase "nested" test_get_nested
        ]
    , testGroup "set" [
          testCase "simple" test_set_simple
        , testCase "nested" test_set_nested
        ]
    ]

{-------------------------------------------------------------------------------
  Example records
-------------------------------------------------------------------------------}

{-# ANN type Person largeRecord #-}
data Person = Person {
      name :: String
    , age  :: Int
    }
  deriving (Show, Eq)

{-# ANN type Company largeRecord #-}
data Company = Company {
      name :: String
    , ceo  :: Person
    }
  deriving (Show, Eq)

person :: Person
person = Person {
      name = "John Doe"
    , age  = 30
    }

company :: Company
company = Company {
      name = "Sprockets Inc"
    , ceo  = person
    }

{-------------------------------------------------------------------------------
  Sanity check we really are dealing with a large-record here
-------------------------------------------------------------------------------}

_lensesPerson :: Rep (SimpleRecordLens Person) Person
_lensesPerson = lensesForSimpleRecord

_lensesCompany :: Rep (SimpleRecordLens Company) Company
_lensesCompany = lensesForSimpleRecord

{-------------------------------------------------------------------------------
  Get fields
-------------------------------------------------------------------------------}

test_get_simple :: Assertion
test_get_simple = assertEqual "" 30 $ person.age

test_get_nested :: Assertion
test_get_nested = assertEqual "" 30 $ company.ceo.age

{-------------------------------------------------------------------------------
  Set fields
-------------------------------------------------------------------------------}

test_set_simple :: Assertion
test_set_simple =
    assertEqual "" expected $
      person{age = 31}
  where
    expected :: Person
    expected = Person {
          name = "John Doe"
        , age  = 31
        }

test_set_nested :: Assertion
test_set_nested =
    assertEqual "" expected $
      company{ceo.age = 31}
  where
    expected :: Company
    expected = Company {
          name = "Sprockets Inc"
        , ceo  = Person {
              name = "John Doe"
            , age  = 31
            }
        }

#endif
