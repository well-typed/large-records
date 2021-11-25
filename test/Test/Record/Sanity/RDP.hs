{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if defined(USE_RDP)
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
#endif

module Test.Record.Sanity.RDP (tests) where

#if !defined(USE_RDP)

import Test.Tasty

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RDP" []

#else

---------------- Tests below only compiled when RDP is enabled ----------------

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Simple field selection and override
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
     data R1 = MkR1 { r1_x :: Int, r1_y :: Bool }
       deriving (Show, Eq)
  |]

test_simple :: Assertion
test_simple = do
    assertEqual "x"     r1.r1_x $ 1
    assertEqual "y"     r1.r1_y $ True
    assertEqual "r1_x"  r1_x    $ r1{ r1_x = 2     }
    assertEqual "r1_y"  r1_y    $ r1{ r1_y = False }
    assertEqual "r1_xy" r1_xy   $ r1{ r1_x = 2, r1_y = False }
  where
    r1, r1_x, r1_y, r1_xy :: R1
    r1    = [lr| MkR1 { r1_x = 1, r1_y = True  } |]
    r1_x  = [lr| MkR1 { r1_x = 2, r1_y = True  } |]
    r1_y  = [lr| MkR1 { r1_x = 1, r1_y = False } |]
    r1_xy = [lr| MkR1 { r1_x = 2, r1_y = False } |]

{-------------------------------------------------------------------------------
  Field selection and override when there are overloaded fields
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
    data R2 = MkR2 { a :: Int, b :: Bool } deriving (Show, Eq)
    data R3 = MkR3 { a :: Int, b :: Char } deriving (Show, Eq)
  |]

test_overloaded :: Assertion
test_overloaded = do
    assertEqual "r2.a" r2.a $ 1
    assertEqual "r2.b" r2.b $ True
    assertEqual "r3.a" r3.a $ 2
    assertEqual "r3.b" r3.b $ 'a'
    assertEqual "r2'"  r2'  $ r2{ a = 2   }
    assertEqual "r3'"  r3'  $ r3{ b = 'b' }
  where
    r2, r2' :: R2
    r2  = [lr| MkR2 { a = 1, b = True } |]
    r2' = [lr| MkR2 { a = 2, b = True } |]

    r3 :: R3
    r3  = [lr| MkR3 { a = 2, b = 'a' } |]
    r3' = [lr| MkR3 { a = 2, b = 'b' } |]

{-------------------------------------------------------------------------------
  Collect all tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RDP" [
      testCase "simple"     test_simple
    , testCase "overloaded" test_overloaded
    ]

#endif