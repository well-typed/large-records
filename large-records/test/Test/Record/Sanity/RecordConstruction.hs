{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.RecordConstruction (tests) where

import GHC.Records.Compat
import Test.Tasty
import Test.Tasty.HUnit

-- Test that this works if we don't generate field accessors
-- See <https://gitlab.haskell.org/ghc/ghc/-/issues/19312>
--
-- Use lazy fields so that we can test values with missing fields.
--
-- Test both the case where the name of the type and the name of the constructor
-- are the same and where they are different.

{-# ANN type R largeRecord #-}
data R a = MkR { x :: Int, y :: [a] } deriving (Eq, Show)

{-# ANN type S largeRecord #-}
data S a = S { x :: Int, y :: [a] } deriving (Eq, Show)

inOrder :: R Bool
inOrder = MkR { x = 1234, y = [True] }

outOfOrder :: R Bool
outOfOrder = MkR { y = [True], x = 1234 }

-- | Constructor application
--
-- Occassionally we cannot use the quasi-quoter (for instance, in an
-- applicative context).
constructorApp :: R Bool
constructorApp = MkR 1234 [True]

valueOfS :: S Bool
valueOfS = S { x = 1234, y = [True] }

{-------------------------------------------------------------------------------
  Nested records
-------------------------------------------------------------------------------}

data RegularRecord = RR { a :: Int }
  deriving (Show, Eq)

{-# ANN type T largeRecord #-}
data T = T { x :: Int, y :: S Bool, z :: RegularRecord }

valueOfT :: T
valueOfT = T { x = 5
             , y = S { x = 1234, y = [True] }
             , z = RR { a = 5 }
             }

{-------------------------------------------------------------------------------
  Sanity check
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.RecordConstruction" [
      testCase "allEqual" testAllEqual
    ]

testAllEqual :: Assertion
testAllEqual = do
    assertEqual "inOrder/outOfOrder" (getField @"x" inOrder)  (getField @"x" outOfOrder)
    assertEqual "inOrder/withoutQQ"  (getField @"x" inOrder)  (getField @"x" constructorApp)
    assertEqual "R/S"                (getField @"x" inOrder)  (getField @"x" valueOfS)
    assertEqual "T/S"                (getField @"y" valueOfT) valueOfS
    assertEqual "T/R"                (getField @"z" valueOfT) (RR 5)
