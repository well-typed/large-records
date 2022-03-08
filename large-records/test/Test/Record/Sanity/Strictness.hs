{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.Strictness (tests) where

import Control.Exception
import GHC.Records.Compat

import Data.Record.Generic
import Data.Record.Generic.LowerBound

import qualified Data.Record.Generic.Rep as Rep

import Test.Tasty
import Test.Tasty.HUnit

{-# ANN type Lazy largeRecordLazy #-}
data Lazy = MkLazy { lazyField :: Word }

{-# ANN type Strict largeRecordStrict #-}
data Strict = MkStrict { strictField :: Word }

_silenceWarnings :: Lazy -> Strict -> ()
_silenceWarnings MkLazy{..} MkStrict{..} = const () $ (
      lazyField
    , strictField
    )

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Strictness" [
      testCase "initValueLazy"   test_initValueLazy
    , testCase "initValueStrict" test_initValueStrict
    , testCase "setValueLazy"    test_setValueLazy
    , testCase "setValueStrict"  test_setValueStrict
    ]

test_initValueLazy :: Assertion
test_initValueLazy =
    initValue `seq` return ()
  where
    initValue :: Lazy
    initValue = to $ Rep.pure undefined

test_initValueStrict :: Assertion
test_initValueStrict =
    expectBoom (initValue `seq` return ())
  where
    initValue :: Strict
    initValue = to $ Rep.pure boom

test_setValueLazy :: Assertion
test_setValueLazy =
    setField @"lazyField" initValue boom `seq` return ()
  where
    initValue :: Lazy
    initValue = glowerBound

test_setValueStrict :: Assertion
test_setValueStrict =
    expectBoom (setField @"strictField" initValue boom `seq` return ())
  where
    initValue :: Strict
    initValue = glowerBound

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

data Boom = Boom
  deriving (Show)

boom :: a
boom = throw Boom

instance Exception Boom

expectBoom :: IO () -> Assertion
expectBoom act = do
    mErr <- try act
    case mErr of
      Left Boom -> return ()
      Right ()  -> assertFailure "Expected boom"
