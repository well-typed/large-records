{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Test.Record.Anonymous.Prop.Combinators.Simple (tests) where

import Control.Monad.State
import Data.Bifunctor
import Data.SOP

import qualified Data.Record.Anonymous as Anon

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Record.Anonymous.Prop.Model.Orphans ()
import Test.Record.Anonymous.Prop.Model.Generator

import qualified Test.Record.Anonymous.Prop.Model as Modl

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Prop.Combinators.Simple" [
      testProperty "map"       test_map
    , testProperty "mapM"      test_mapM
    , testProperty "zip"       test_zip
    , testProperty "zipWith"   test_zipWith
    , testProperty "zipWithM"  test_zipWithM
    , testProperty "collapse"  test_collapse
    , testProperty "sequenceA" test_sequenceA
    , testProperty "pure"      test_pure
    , testProperty "ap"        test_ap
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_map ::
     SomeRecord (K Int)
  -> Fun Int Int
  -> Property
test_map r (applyFun -> f) =
        onModlRecord (Modl.map f') r
    === onAnonRecord (Anon.map f') r
  where
    f' :: K Int x -> K Int x
    f' = mapKK f

test_mapM ::
     SomeRecord (K Int)
  -> Fun (Int, Word) (Int, Word)
  -> Property
test_mapM r (applyFun -> f) =
        (run $ onModlRecordM (Modl.mapM f') r)
    === (run $ onAnonRecordM (Anon.mapM f') r)
  where
    run :: State Word a -> a
    run = flip evalState 0

    f' :: K Int x -> State Word (K Int x)
    f' (K x) = state $ \s -> first K $ f (x, s)

test_zip ::
     SomeRecordPair (K Int) (K Int)
  -> Property
test_zip r =
        onModlRecordPair Modl.zip r
    === onAnonRecordPair Anon.zip r

test_zipWith ::
     SomeRecordPair (K Int) (K Int)
  -> Fun (Int, Int) Int
  -> Property
test_zipWith r (applyFun -> f) =
        onModlRecordPair (Modl.zipWith f') r
    === onAnonRecordPair (Anon.zipWith f') r
  where
    f' :: K Int x -> K Int x -> K Int x
    f' (K x) (K y) = K $ f (x, y)

test_zipWithM ::
     SomeRecordPair (K Int) (K Int)
  -> Fun (Int, Int, Word) (Int, Word)
  -> Property
test_zipWithM r (applyFun -> f) =
        (run $ onModlRecordPairM (Modl.zipWithM f') r)
    === (run $ onAnonRecordPairM (Anon.zipWithM f') r)
  where
    run :: State Word a -> a
    run = flip evalState 0

    f' :: K Int x -> K Int x -> State Word (K Int x)
    f' (K x) (K y) = state $ \s -> first K $ f (x, y, s)

test_collapse ::
     SomeRecord (K Int)
  -> Property
test_collapse (SR mf r) =
        Modl.collapse r
    === Anon.collapse (Modl.toRecord mf r)

test_sequenceA ::
     SomeRecord (K Int)
  -> Fun (Int, Word) (Int, Word)
  -> Property
test_sequenceA r (applyFun -> f) =
        (run $ onModlRecordM Modl.sequenceA r')
    === (run $ onAnonRecordM Anon.sequenceA r')
  where
    run :: State Word a -> a
    run = flip evalState 0

    r' :: SomeRecord (State Word :.: K Int)
    r' = onModlRecord (Modl.map f') r

    f' :: K Int x -> (State Word :.: K Int) x
    f' (K x) = Comp $ state $ \s -> first K $ f (x, s)

test_pure :: SomeFields -> Property
test_pure sf =
        someModlRecord sf (\mf -> Modl.pure mf (K True))
    === someAnonRecord sf (       Anon.pure    (K True))

test_ap ::
     SomeRecordPair (K Int) (K Int)
  -> Property
test_ap (SR2 mf rx ry) =
        onModlRecordPair Modl.ap r'
    === onAnonRecordPair Anon.ap r'
  where
    r' :: SomeRecordPair (K Int -.-> K Int) (K Int)
    r' = SR2 mf (Modl.map f rx) ry

    f :: K Int x -> (K Int -.-> K Int) x
    f (K x) = fn $ \(K y) -> K (x + y)
