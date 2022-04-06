{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

-- | Sanity checks of the TH code generation
module Test.Record.Sanity.CodeGen (tests) where

import Data.Record.Generic

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Test record

  This is the example record we use throughout the comments in TH codegen.
-------------------------------------------------------------------------------}

{-# ANN type T largeRecord #-}
data T a b = MkT {
      tInt   :: Word
    , tBool  :: Bool
    , tChar  :: Char
    , tA     :: a
    , tListB :: [b]
    }
  deriving (Eq, Ord, Show)

exampleT :: T () Float
exampleT = MkT 5 True 'c' () [3.14]

_silenceWarnings :: T a b -> ()
_silenceWarnings MkT{..} = const () $ (
      tInt
    , tBool
    , tChar
    , tA
    , tListB
    )

{-------------------------------------------------------------------------------
  Tests

  TODO: Should we have some other sanity checks here of the codegen?
  (They might exist in other parts of the test suite, perhaps we should
  reorganize things a bit.)
-------------------------------------------------------------------------------}

test_from_to_id :: Assertion
test_from_to_id =
    assertEqual "from . to = id" expected actual
  where
    expected, actual :: T () Float
    expected = exampleT
    actual   = (to . from) exampleT

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.CodeGen" [
      testCase "from_to_id" test_from_to_id
    ]
