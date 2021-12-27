{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.QualifiedImports (tests) where

import GHC.Records.Compat

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Record.Sanity.QualifiedImports.A as A
import qualified Test.Record.Sanity.QualifiedImports.B as B

constructA :: A.T Bool
constructA = A.MkT { A.x = 5, A.y = [True] }

constructB :: B.T Bool
constructB = B.MkT { B.x = 'a', B.y = A.MkT { A.x = 2, A.y = [True, False] } }

projectA :: A.T a -> (Int, [a])
projectA A.MkT { A.x = a, A.y = b } = (a, b)

projectB :: B.T a -> (Char, Int, [a])
projectB B.MkT { B.x = a, B.y = A.MkT { A.x = b, A.y = c } } = (a, b, c)

tests :: TestTree
tests = testGroup "Test.Record.Sanity.QualifiedImports" [
      testCase "qualifiedImports" testQualifiedImports
    ]

testQualifiedImports :: Assertion
testQualifiedImports = do
    assertEqual "projectA"   (projectA a)      $ (5, [True])
    assertEqual "projectB"   (projectB b)      $ ('a', 2, [True, False])
    assertEqual "constructA" (getField @"x" a) $ 5
    assertEqual "constructB" (getField @"x" b) $ 'a'
  where
    a :: A.T Bool
    a = constructA

    b :: B.T Bool
    b = constructB
