{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.Intersection (tests) where

import Data.SOP.BasicFunctors

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

import Test.Tasty
import Test.Tasty.HUnit

import Test.Infra.Discovery

tests :: TestTree
tests = testGroup "Test.Sanity.Intersection" [
      testCase "intersection" test_intersection
    ]

test_intersection :: Assertion
test_intersection =
    case intersect example1 example2 of
      Intersection p -> go p
  where
    go :: forall ri.
         (Project Row1 ri, Project Row2 ri)
      => Proxy ri -> Assertion
    go _ = do
        assertEqual "1" example1' $
          Anon.inject projected2 example1
        assertEqual "2" example2' $
          Anon.inject projected1 example2
      where
        projected1, projected2 :: Record I ri
        projected1 = Anon.project example1
        projected2 = Anon.project example2

{-------------------------------------------------------------------------------
  Example values

  Row1 only have field "a" in common: field "b" is absent in Row2,
  and field "c" has a different type
-------------------------------------------------------------------------------}

type Row1 = [ "a" := Int, "b" := Bool, "c" := Char ]
type Row2 = [ "c" := Double, "a" := Int ]

example1, example1' :: Record I Row1
example1 = ANON_F {
      a = I 1
    , b = I True
    , c = I 'a'
    }
example1' =  ANON_F {
      a = I 2
    , b = I True
    , c = I 'a'
    }

example2, example2' :: Record I Row2
example2 = ANON_F {
      c = I 3.14
    , a = I 2
    }
example2' = ANON_F {
      c = I 3.14
    , a = I 1
    }