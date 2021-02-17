{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Test.Record.Sanity.PatternMatch (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int,  y :: [a] }
    data S a = MkS { x :: Char, y :: T a }
  |]

endOfBindingGroup

projectOne :: T Bool -> Int
projectOne [lr| MkT { x = a } |] = a

projectTwo :: T a -> (Int, [a])
projectTwo [lr| MkT { x = a, y = b } |] = (a, b)

projectNested :: S a -> (Char, Int, [a])
projectNested [lr| MkS { x = a, y = MkT { x = b, y = c } } |] = (a, b, c)

testProjections :: Assertion
testProjections = do
    assertEqual "projectOne" (projectOne t)  5
    assertEqual "projectTwo" (projectTwo t) (5, [True])
    assertEqual "projectNested" (projectNested s) ('a', 2, [True, False])
  where
    t :: T Bool
    t = [lr| MkT { x = 5, y = [True] } |]

    s :: S Bool
    s = [lr| MkS { x = 'a', y = MkT { x = 2, y = [True, False] } } |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Projection" [
      testCase "projections" testProjections
    ]
