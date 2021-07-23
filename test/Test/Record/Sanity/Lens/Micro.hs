{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Test.Record.Sanity.Lens.Micro (tests) where

import Data.Maybe
import Data.SOP
import Test.Tasty
import Test.Tasty.HUnit
import Lens.Micro

import Data.Record.Generic
import Data.Record.Generic.Lens.Micro
import Data.Record.Generic.SOP
import Data.Record.TH

largeRecord defaultPureScript [d|
      data Example = MkExample {
            e1 :: Int
          , e2 :: Bool
          , e3 :: Char
          }
        deriving (Show, Eq)
    |]

endOfBindingGroup

example :: Example
example = [lr| MkExample {
      e1 = 5
    , e2 = True
    , e3 = 'a'
    } |]

exampleLenses :: NP (Field (MicroLens Example)) (MetadataOf Example)
exampleLenses = fromJust $ toSOP glenses

Field (MicroLens xe1) :* Field (MicroLens xe2) :* Field (MicroLens xe3) :* Nil = exampleLenses

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Lens.Micro" [
      testCase "get" test_get
    , testCase "set" test_set
    ]

test_get :: Assertion
test_get = assertEqual "" True (example ^. xe2)

test_set :: Assertion
test_set = assertEqual "" expected $ example & xe1 %~ negate & xe3 %~ succ
  where
    expected :: Example
    expected = [lr| MkExample {
          e1 = (-5)
        , e2 = True
        , e3 = 'b'
        } |]
