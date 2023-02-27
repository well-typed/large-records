{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- The point of this module is to verify that largeRecord does not generate
-- redundant constraints
{-# OPTIONS_GHC -Werror -Wredundant-constraints #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.EqualFieldTypes (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Plugin

{-# ANN type R largeRecord #-}
data R a = MkR {
      field1 :: a
    , field2 :: a
    }
  deriving (Show, Eq)

swap :: R a -> R a
swap MkR{ field1, field2 } = MkR{ field1 = field2, field2 = field1 }

tests :: TestTree
tests = testGroup "Test.Record.Sanity.EqualFieldTypes" [
      testCase "sanity" test_sanity
    ]

test_sanity :: Assertion
test_sanity = assertEqual "" expected actual
  where
    expected, actual :: R Int
    expected = MkR 2 1
    actual   = swap $ MkR 1 2



