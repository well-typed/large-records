{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.Derive (tests) where

import Control.Newtype
import Data.Functor.Identity
import Data.Kind
import GHC.Records.Compat
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Plugin

{-------------------------------------------------------------------------------
  Class of kind @Type -> Constraint@.
-------------------------------------------------------------------------------}

class C1 (a :: Type) where
  c1 :: a -> String
  c1 _ = "x"

-- Type of kind @Type@

{-# ANN type LA1 largeRecord #-}
data LA1 = MkLA1 { la1_f1 :: Int }
  deriving anyclass C1

-- Type of kind @Type -> Type@ (regular and large example)

data RA2 a = MkRA1 { ra2_f1 :: a }
  deriving anyclass C1

{-# ANN type LA2 largeRecord #-}
data LA2 a = MkLA2 { la2_f1 :: a }
  deriving anyclass C1

{-------------------------------------------------------------------------------
  Class of kind @Type -> Type -> Constraint@, partially instantiated.
  (Purely from a kind point of view, this is no different to the previous
  section, of course.)

  To make transpilation easier, the PureScript to Haskell transpiler makes
  all records as being a "newtype of themselves" (in PureScript, something like

  > newtype T = MkT { a :: Int, b :: Bool }

  really is a newtype around a record).
-------------------------------------------------------------------------------}

{-# ANN type LB largeRecord #-}
data LB = MkLB { lb1 :: Int, lb2 :: Int }
    deriving anyclass (Newtype LB)

f :: LB -> LB
f r = flip (setField @"lb1") (getField @"lb2" r)
    . flip (setField @"lb2") (getField @"lb1" r)
    $ r

{-------------------------------------------------------------------------------
  Class of kind @(Type -> Type) -> Constraint@
-------------------------------------------------------------------------------}

class C3 (f :: Type -> Type) where
  c3 :: f Bool -> String
  c3 _ = "x"

-- Regular (non-large) example
data RC a = MkRC { rc1 :: a, rc2 :: Int }
  deriving anyclass C3

{-# ANN type LC largeRecord #-}
data LC (a :: Type) = MkLC { lc1 :: a, lc2 :: Int }
  deriving anyclass C3

{-------------------------------------------------------------------------------
  Class of kind @((Type -> Type) -> Type) -> Constraint@
-------------------------------------------------------------------------------}

class C4 (f :: (Type -> Type) -> Type) where
  c4 :: f Identity -> String
  c4 _ = "x"

-- Regular (non-large) example
data RD f = MkRD { rd1 :: f Int, rd2 :: Int }
  deriving anyclass C4

{-# ANN type LD largeRecord #-}
data LD (f :: Type -> Type) = MkLD { ld1 :: f Int, ld2 :: Int }
  deriving anyclass C4

{-------------------------------------------------------------------------------
  Class with a constraint
-------------------------------------------------------------------------------}

class Show a => C5 (a :: Type) where
  c5 :: a -> String
  c5 = show

-- Regular (non-large) example
data RE = MkRE { re1 :: Int, re2 :: Bool }
  deriving stock Show
  deriving anyclass C5

{-# ANN type LE largeRecord #-}
data LE = MkLE { le1 :: Int, le2 :: Bool }
  deriving stock Show
  deriving anyclass C5

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Derive" [
      testCase "kind 'Type -> Constraint'"                            test_c1
    , testCase "kind 'Type -> Type -> Constraint', partially applied" test_newtype
    , testCase "kind '(Type -> Type) -> Constraint'"                  test_c3
    , testCase "kind '((Type -> Type) -> Type) -> Constraint'"        test_c4
    , testCase "kind 'Type -> Constraint', superclass constraint"     test_c5
    ]

test_c1 :: Assertion
test_c1 = do
    assertEqual "type of kind 'Type'"         (c1 la1) "x"
    assertEqual "type of kind 'Type -> Type'" (c1 la2) "x"
  where
    la1 :: LA1
    la1 = MkLA1 { la1_f1 = 1 }

    la2 :: LA2 Int
    la2 = MkLA2 { la2_f1 = 1 }

test_newtype :: Assertion
test_newtype =
    assertEqual "" (getField @"lb1" r') 2
  where
    r :: LB
    r =  MkLB { lb1 = 1, lb2 = 2 }

    r' :: LB
    r' = f r

test_c3 :: Assertion
test_c3 =
    assertEqual "" (c3 r) "x"
  where
    r :: LC Bool
    r = MkLC { lc1 = True, lc2 = 2 }

test_c4 :: Assertion
test_c4 =
    assertEqual "" (c4 r) "x"
  where
    r :: LD Identity
    r = MkLD { ld1 = Identity 1, ld2 = 2 }

test_c5 :: Assertion
test_c5 =
    assertEqual "" (c5 r) "MkLE {le1 = 1, le2 = True}"
  where
    r :: LE
    r = MkLE { le1 = 1, le2 = True }
