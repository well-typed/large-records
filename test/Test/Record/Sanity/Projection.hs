{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Record.Sanity.Projection (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH

largeRecord defaultPureScript [d|
    data T a = MkT { x :: Int, y :: [a] }
  |]

endOfBindingGroup

projectOne :: T Bool -> Int
projectOne [lr| MkT { x = a } |] = a

projectTwo :: T a -> (Int, [a])
projectTwo [lr| MkT { x = a, y = b } |] = (a, b)

testProjections :: Assertion
testProjections = do
    assertEqual "projectOne" (projectOne t) 5
    assertEqual "projectTwo" (projectTwo t) (5, [True])
  where
    t :: T Bool
    t = [lr| MkT { x = 5, y = [True] } |]

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Projection" [
      testCase "projections" testProjections
    ]
