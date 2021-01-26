{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -ddump-splices #-}

#if PROFILE_GEN_CODE
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin #-}
#endif

module Test.Record.Generic.Size.InstanceConstraints (
    tests
  ) where

import Data.Kind
import GHC.TypeLits

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.Generic.TH

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
  |]

instance (
    Show (T 0 f)
  , Show (T 1 f)
  , Show (T 2 f)
  , Show (T 3 f)
  , Show (T 4 f)
  , Show (T 5 f)
  , Show (T 6 f)
  , Show (T 7 f)
  , Show (T 8 f)
  , Show (T 9 f)
  ) => Show (MyRecord f) where
  show r = show (
      field0 r
    , field1 r
    , field2 r
    , field3 r
    , field4 r
    , field5 r
    , field6 r
    , field7 r
    , field8 r
    , field9 r
    )

example :: MyRecord I
example = glowerBound

{-------------------------------------------------------------------------------
  Just to make sure we don't regard everything as dead code
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Size.InstanceConstraints" [
      testCase "show" test_show
    ]

test_show :: Assertion
test_show = assertEqual "" "" (show example)
