{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.HKD (
    tests
  ) where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Kind
import GHC.Records.Compat

import Test.Tasty
import Test.Tasty.HUnit

type family HKD f a where
  HKD Identity  a = a
  HKD (Const b) a = b

-- | Test record with fields whose types are given by type families
{-# ANN type T largeRecord #-}
data T (f :: Type -> Type) = MkT {
      field1 :: HKD f Int
    , field2 :: HKD f Bool
    }
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.HKD" [
      testCase "get"   testGet
    , testCase "set"   testSet
    , testCase "match" testMatch
    ]

example1, example2 :: T Identity
example1 = MkT { field1 = 1, field2 = True  }
example2 = MkT { field1 = 1, field2 = False }

example3 :: T (Const Char)
example3 = MkT { field1 = 'a', field2 = 'b' }

exampleFun :: T f -> HKD f Int
exampleFun MkT { field1 } = field1

testGet, testSet :: Assertion
testGet = assertEqual "" (getField @"field1" example1) 1
testSet = assertEqual "" (setField @"field2" example2 False) example2

testMatch :: Assertion
testMatch = do
    assertEqual "example1" (exampleFun example1) 1
    assertEqual "example3" (exampleFun example3) 'a'
