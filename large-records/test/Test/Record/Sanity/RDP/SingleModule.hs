{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

-- | Test what happens if both plugins are used in the same module
module Test.Record.Sanity.RDP.SingleModule (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Plugin

{-------------------------------------------------------------------------------
  Simple field selection and override
-------------------------------------------------------------------------------}

{-# ANN type R1 largeRecord #-}
data R1 = MkR1 { r1_x :: Int, r1_y :: Bool }
  deriving (Show, Eq)

test_simple :: Assertion
test_simple = do
    assertEqual "x"     r1.r1_x $ 1
    assertEqual "y"     r1.r1_y $ True
    assertEqual "r1_x"  r1_x'   $ r1{r1_x = 2}
    assertEqual "r1_y"  r1_y'   $ r1{r1_y = False}
    assertEqual "r1_xy" r1_xy'  $ r1{r1_x = 2, r1_y = False}
  where
    r1, r1_x', r1_y', r1_xy' :: R1
    r1     = MkR1 { r1_x = 1, r1_y = True  }
    r1_x'  = MkR1 { r1_x = 2, r1_y = True  }
    r1_y'  = MkR1 { r1_x = 1, r1_y = False }
    r1_xy' = MkR1 { r1_x = 2, r1_y = False }

{-------------------------------------------------------------------------------
  Field selection and override when there are overloaded fields
-------------------------------------------------------------------------------}

{-# ANN type R2 largeRecord #-}
data R2 = MkR2 { a :: Int, b :: Bool }
  deriving (Show, Eq)

{-# ANN type R3 largeRecord #-}
data R3 = MkR3 { a :: Int, b :: Char }
  deriving (Show, Eq)

test_overloaded :: Assertion
test_overloaded = do
    assertEqual "r2.a" r2.a $ 1
    assertEqual "r2.b" r2.b $ True
    assertEqual "r3.a" r3.a $ 2
    assertEqual "r3.b" r3.b $ 'a'
    assertEqual "r2'"  r2'  $ r2{a = 2}
    assertEqual "r3'"  r3'  $ r3{b = 'b'}
  where
    r2, r2' :: R2
    r2  = MkR2 { a = 1, b = True }
    r2' = MkR2 { a = 2, b = True }

    r3 :: R3
    r3  = MkR3 { a = 2, b = 'a' }
    r3' = MkR3 { a = 2, b = 'b' }

{-------------------------------------------------------------------------------
  Nested records

  Both with and without LR.
-------------------------------------------------------------------------------}

data R4_WOutLR = MkR4_WOutLR { r4_woutLR_x :: Int, r4_woutLR_y :: R5_WOutLR }
  deriving (Show, Eq)

data R5_WOutLR = MkR5_WOutLR { r5_woutLR_x :: Char, r5_woutLR_y :: Double }
  deriving (Show, Eq)

{-# ANN type R4_WithLR largeRecord #-}
data R4_WithLR = MkR4_WithLR { r4_withLR_x :: Int, r4_withLR_y :: R5_WithLR }
  deriving (Show, Eq)

{-# ANN type R5_WithLR largeRecord #-}
data R5_WithLR = MkR5_WithLR { r5_withLR_x :: Char, r5_withLR_y :: Double }
  deriving (Show, Eq)

test_nested :: Assertion
test_nested = do
    assertEqual "r4_woutLR_x" r4_woutLR.r4_woutLR_y.r5_woutLR_x $ 'a'
    assertEqual "r4_withLR_x" r4_WithLR.r4_withLR_y.r5_withLR_x $ 'a'
  where
    r4_woutLR :: R4_WOutLR
    r4_woutLR = MkR4_WOutLR { r4_woutLR_x = 1, r4_woutLR_y = r5_woutLR }

    r5_woutLR :: R5_WOutLR
    r5_woutLR = MkR5_WOutLR { r5_woutLR_x = 'a', r5_woutLR_y = 1.2 }

    r4_WithLR :: R4_WithLR
    r4_WithLR = MkR4_WithLR { r4_withLR_x = 1, r4_withLR_y = r5_WithLR }

    r5_WithLR :: R5_WithLR
    r5_WithLR = MkR5_WithLR { r5_withLR_x = 'a', r5_withLR_y = 1.2 }

{-------------------------------------------------------------------------------
  Collect all tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RDP" [
      testCase "simple"     test_simple
    , testCase "overloaded" test_overloaded
    , testCase "nested"     test_nested
    ]
