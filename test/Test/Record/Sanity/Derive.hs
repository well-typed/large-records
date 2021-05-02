{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Test.Record.Sanity.Derive where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.TH
import Control.Newtype

wrap :: Newtype a b => b -> a
wrap = pack

unwrap :: Newtype a b => a -> b
unwrap = unpack

largeRecord defaultPureScript [d|
    data R = R { a :: Int, b :: Int }
        deriving anyclass (Newtype(R))
    |]

endOfBindingGroup

f :: R -> R
f r = wrap ((unwrap r){ a = 1, b = 1 })

testDeriveNewtype :: Assertion
testDeriveNewtype = do
   let r =  [lr| R { a = 2, b = 2 }|]
   let r' = f r
   assertEqual "pack/unpack" r'.a 1

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Derive" [
      testCase "projections" testDeriveNewtype
    ]

