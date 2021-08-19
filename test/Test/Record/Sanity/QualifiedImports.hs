{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.QualifiedImports (tests) where

import Data.Record.TH

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Record.Sanity.QualifiedImports.A as A
import qualified Test.Record.Sanity.QualifiedImports.B as B

constructA :: A.T Bool
constructA = [lr| A.MkT { x = 5, y = [True] } |]

constructB :: B.T Bool
constructB = [lr| B.MkT { x = 'a', y = A.MkT { x = 2, y = [True, False] } } |]

projectA :: A.T a -> (Int, [a])
projectA [lr| A.MkT { x = a, y = b } |] = (a, b)

projectB :: B.T a -> (Char, Int, [a])
projectB [lr| B.MkT { x = a, y = A.MkT { x = b, y = c } } |] = (a, b, c)

tests :: TestTree
tests = testGroup "Test.Record.Sanity.QualifiedImports" [
      testCase "qualifiedImports" testQualifiedImports
    ]

testQualifiedImports :: Assertion
testQualifiedImports = do
    assertEqual "constructA" a.x          5
    assertEqual "projectA"   (projectA a) (5, [True])
    assertEqual "constructB" b.x          'a'
    assertEqual "projectB"   (projectB b) ('a', 2, [True, False])
  where
    a :: A.T Bool
    a = constructA

    b :: B.T Bool
    b = constructB
