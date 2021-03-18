{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module Test.Record.Sanity.PatternMatch (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH

data R a = MkR { rInt :: Int,  rList :: [a] }

_projectPunsR :: R a -> (Int, [a])
_projectPunsR MkR { rInt, rList } = (rInt, rList)

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int,  y :: [a] }
    data S a = MkS { x :: Char, y :: T a }
  |]

endOfBindingGroup

projectOne :: T Bool -> Int
projectOne [lr| MkT { x = a } |] = a

projectTwo :: T a -> (Int, [a])
projectTwo [lr| MkT { x = a, y = b } |] = (a, b)

projectPuns :: T a -> (Int, [a])
projectPuns [lr| MkT { x, y } |] = (x, y)

projectNested :: S a -> (Char, Int, [a])
projectNested [lr| MkS { x = a, y = MkT { x = b, y = c } } |] = (a, b, c)

projectView :: T Bool -> Int
projectView [lr| MkT { x = ((+1) -> a) } |] = a

testProjections :: Assertion
testProjections = do
    assertEqual "one"    (projectOne    t)  5
    assertEqual "two"    (projectTwo    t) (5, [True])
    assertEqual "puns"   (projectPuns   t) (5, [True])
    assertEqual "nested" (projectNested s) ('a', 2, [True, False])
    assertEqual "view"   (projectView   t)  6
  where
    t :: T Bool
    t = [lr| MkT { x = 5, y = [True] } |]

    s :: S Bool
    s = [lr| MkS { x = 'a', y = MkT { x = 2, y = [True, False] } } |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Projection" [
      testCase "projections" testProjections
    ]
