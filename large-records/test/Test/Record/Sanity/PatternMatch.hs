{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors  #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.PatternMatch (tests) where

import Control.Exception
import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit

import Test.Record.Util

{-------------------------------------------------------------------------------
  Basic pattern matching tests
-------------------------------------------------------------------------------}

{-# ANN type T largeRecordStrict #-}
data T a = MkT { x :: Int,  y :: [a], z :: Double }

{-# ANN type S largeRecordStrict #-}
data S a = MkS { x :: Char, y :: T a }

projectOne :: T Bool -> Int
projectOne MkT { x = a } = a

projectTwo :: T a -> (Int, [a])
projectTwo MkT { x = a, y = b } = (a, b)

-- | Test projecting more than 2 elements
--
-- This is an important special case, because this checks that the pattern we
-- generate is correctedly nested to match the 'MatchHasField' instances.
projectThree :: T a -> (Int, [a], Double)
projectThree MkT { x = a, y = b, z = c } = (a, b, c)

projectPuns :: T a -> (Int, [a])
projectPuns MkT { x, y } = (x, y)

projectNested :: S a -> (Char, Int, [a])
projectNested MkS { x = a, y = MkT { x = b, y = c } } = (a, b, c)

projectView :: T Bool -> Int
projectView MkT { x = ((+1) -> a) } = a

matchEmpty :: T Bool -> Int
matchEmpty MkT {} = 42

-- | A pattern match on a record that does not extract any variables should
-- nonetheless be strict
matchEmptyUndefined :: Int
matchEmptyUndefined = matchEmpty (error "boom")

{-------------------------------------------------------------------------------
  Verify inferred types

  We want to infer that the types are not more polymorphic than they should be:
  functions that match on a record should not be polymorphic in 'HasField', but
  should only accept values of that specific record type.

  Functions 'useNoSigEmpty' and 'useNoSigNonEmpty' below will have (deferred)
  type errors iff 'noSigEmpty' and 'noSigNonEmpty' are suffciently monomorphic.
-------------------------------------------------------------------------------}

{-# ANN type T2 largeRecordStrict #-}
data T2 = MkT2 { x :: Int }

noSigEmpty MkT {} = ()

noSigNonEmpty MkT { x = a } = const () a

useNoSigEmpty :: ()
useNoSigEmpty = noSigEmpty MkT2 { x = 5 }

useNoSigNonEmpty :: ()
useNoSigNonEmpty = noSigNonEmpty MkT2 { x = 5 }

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

testProjections :: Assertion
testProjections = do
    assertEqual "one"    (projectOne    t)  5
    assertEqual "two"    (projectTwo    t) (5, [True])
    assertEqual "three"  (projectThree  t) (5, [True], 1.0)
    assertEqual "puns"   (projectPuns   t) (5, [True])
    assertEqual "nested" (projectNested s) ('a', 2, [True, False])
    assertEqual "view"   (projectView   t)  6
    assertEqual "empty"  (matchEmpty    t)  42

    expectException isBoom $
      assertEqual "empty-undefined" matchEmptyUndefined 42

    expectException isExpectedTypeError $
      assertEqual "sig-empty"    useNoSigEmpty    ()
    expectException isExpectedTypeError $
      assertEqual "sig-nonempty" useNoSigNonEmpty ()
  where
    isBoom :: SomeException -> Bool
    isBoom e = "boom" `isInfixOf` show e

    isExpectedTypeError :: SomeException -> Bool
    isExpectedTypeError e = and [
        "Couldn't match" `isInfixOf` show e
      , "T"              `isInfixOf` show e
      , "T2"             `isInfixOf` show e
      ]

    t :: T Bool
    t = MkT { x = 5, y = [True], z = 1.0 }

    s :: S Bool
    s = MkS { x = 'a', y = MkT { x = 2, y = [True, False], z = 1.0 } }

tests :: TestTree
tests = testGroup "Test.Record.Sanity.PatternMatch" [
      testCase "projections" testProjections
    ]
