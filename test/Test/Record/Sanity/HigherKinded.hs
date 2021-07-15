{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

#if PROFILE_GEN_CODE
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

-- | Simple example of a type with a higher kind
module Test.Record.Sanity.HigherKinded (
    tests
  ) where

import Data.Kind
import GHC.TypeLits

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

newtype T (n :: Nat) (f :: Type -> Type) = MkT (f Word)

instance LowerBound (T n I) where
  lowerBound = MkT (I lowerBound)

deriving instance Show (T n I)

-- We need an explicit kind annotation on @f@ for @large-records@ to generate
-- correct code (either that, or use @PolyKinds@).
largeRecord defaultLazyOptions [d|
  data MyRecord (f :: Type -> Type) = MyRecord {
        field0 :: T 0 f
      , field1 :: T 1 f
      , field2 :: T 2 f
      , field3 :: T 3 f
      , field4 :: T 4 f
      , field5 :: T 5 f
      , field6 :: T 6 f
      , field7 :: T 7 f
      , field8 :: T 8 f
      , field9 :: T 9 f
      }
      deriving (Show)
  |]

_suppressWarnings :: ()
_suppressWarnings = const () (
      field0
    , field1
    , field2
    , field3
    , field4
    , field5
    , field6
    , field7
    , field8
    , field9
    )

example :: MyRecord I
example = glowerBound

{-------------------------------------------------------------------------------
  Just to make sure we don't regard everything as dead code
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.HigherKinded" [
      testCase "show" test_show
    ]

test_show :: Assertion
test_show = assertBool "" $ (not . null) (show example)
