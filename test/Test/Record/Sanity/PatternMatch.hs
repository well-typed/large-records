{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors  #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.PatternMatch (tests) where

import Control.Exception
import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH

import Test.Record.Util

{-------------------------------------------------------------------------------
  Basic pattern matching tests
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int,  y :: [a], z :: Double }
    data S a = MkS { x :: Char, y :: T a }
  |]

projectOne :: T Bool -> Int
projectOne [lr| MkT { x = a } |] = a

projectTwo :: T a -> (Int, [a])
projectTwo [lr| MkT { x = a, y = b } |] = (a, b)

-- | Test projecting more than 2 elements
--
-- This is an important special case, because this checks that the pattern we
-- generate is correctedly nested to match the 'MatchHasField' instances.
projectThree :: T a -> (Int, [a], Double)
projectThree [lr| MkT { x = a, y = b, z = c } |] = (a, b, c)

projectPuns :: T a -> (Int, [a])
projectPuns [lr| MkT { x, y } |] = (x, y)

projectNested :: S a -> (Char, Int, [a])
projectNested [lr| MkS { x = a, y = MkT { x = b, y = c } } |] = (a, b, c)

projectView :: T Bool -> Int
projectView [lr| MkT { x = ((+1) -> a) } |] = a

matchEmpty :: T Bool -> Int
matchEmpty [lr| MkT {} |] = 42

{-------------------------------------------------------------------------------
  Verify inferred types

  We want to infer that the types are not more polymorphic than they should be:
  functions that match on a record should not be polymorphic in 'HasField', but
  should only accept values of that specific record type.

  Functions 'useNoSigEmpty' and 'useNoSigNonEmpty' below will have (deferred)
  type errors iff 'noSigEmpty' and 'noSigNonEmpty' are suffciently monomorphic.
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
    data T2 = MkT2 { x :: Int }
  |]

noSigEmpty [lr| MkT {} |] = ()

noSigNonEmpty [lr| MkT { x = a } |] = const () a

useNoSigEmpty :: ()
useNoSigEmpty = noSigEmpty [lr| MkT2 { x = 5 } |]

useNoSigNonEmpty :: ()
useNoSigNonEmpty = noSigNonEmpty [lr| MkT2 { x = 5 } |]

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

    expectException isExpectedTypeError $
      assertEqual "sig-empty"    useNoSigEmpty    ()
    expectException isExpectedTypeError $
      assertEqual "sig-nonempty" useNoSigNonEmpty ()
  where
    isExpectedTypeError :: SomeException -> Bool
    isExpectedTypeError e = "Couldn't match expected type" `isInfixOf` show e

    t :: T Bool
    t = [lr| MkT { x = 5, y = [True], z = 1.0 } |]

    s :: S Bool
    s = [lr| MkS { x = 'a', y = MkT { x = 2, y = [True, False], z = 1.0 } } |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Projection" [
      testCase "projections" testProjections
    ]
