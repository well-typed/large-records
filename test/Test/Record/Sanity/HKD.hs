{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

#if USE_RDP
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
#endif

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.HKD (
    tests
  ) where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Kind

#if !USE_RDP
import GHC.Records.Compat
#endif

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

type family HKD f a where
  HKD Identity  a = a
  HKD (Const b) a = b

-- | Test record with fields whose types are given by type families
largeRecord defaultPureScript [d|
    data T (f :: Type -> Type) = MkT {
          field1 :: HKD f Int
        , field2 :: HKD f Bool
        }
      deriving (Show, Eq)
  |]

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
example1 = [lr| MkT { field1 = 1, field2 = True  } |]
example2 = [lr| MkT { field1 = 1, field2 = False } |]

example3 :: T (Const Char)
example3 = [lr| MkT { field1 = 'a', field2 = 'b' } |]

exampleFun :: T f -> HKD f Int
exampleFun [lr| MkT { field1 } |] = field1

testGet, testSet :: Assertion

#if USE_RDP
testGet = assertEqual "" example1.field1 1
testSet = assertEqual "" (example1{ field2 = False }) example2
#else
testGet = assertEqual "" (getField @"field1" example1) 1
testSet = assertEqual "" (setField @"field2" example2 False) example2
#endif

testMatch :: Assertion
testMatch = do
    assertEqual "example1" (exampleFun example1) 1
    assertEqual "example3" (exampleFun example3) 'a'
