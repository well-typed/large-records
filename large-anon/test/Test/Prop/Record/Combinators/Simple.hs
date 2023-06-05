{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Prop.Record.Combinators.Simple (tests) where

import Control.Monad.State
import Data.Bifunctor
import Data.SOP

import qualified Data.Record.Anon          as Anon
import qualified Data.Record.Anon.Advanced as Anon

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Prop.Record.Model.Orphans ()
import Test.Prop.Record.Model.Generator

import qualified Test.Prop.Record.Model as Modl

tests :: TestTree
tests = testGroup "Test.Prop.Record.Combinators.Simple" [
      testProperty "map"        test_map
    , testProperty "mapM"       test_mapM
    , testProperty "zip"        test_zip
    , testProperty "zipWith"    test_zipWith
    , testProperty "zipWithM"   test_zipWithM
    , testProperty "collapse"   test_collapse
    , testProperty "sequenceA"  test_sequenceA
    , testProperty "pure"       test_pure
    , testProperty "ap"         test_ap
    , testProperty "distribute" test_distribute
    ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

pTop :: Proxy Top
pTop = Proxy

data Pair a = MkPair a a
  deriving (Show, Eq, Functor)

instance Applicative Pair where
  pure x = MkPair x x
  MkPair f1 f2 <*> MkPair x1 x2 = MkPair (f1 x1) (f2 x2)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_map ::
     SomeRecord (K Int)
  -> Fun Int Int
  -> Property
test_map r (applyFun -> f) =
        onModlRecord pTop (Modl.map f') r
    === onAnonRecord pTop (Anon.map f') r
  where
    f' :: K Int x -> K Int x
    f' = mapKK f

test_mapM ::
     SomeRecord (K Int)
  -> Fun (Int, Word) (Int, Word)
  -> Property
test_mapM r (applyFun -> f) =
        (run $ onModlRecordM pTop (Modl.mapM f') r)
    === (run $ onAnonRecordM pTop (Anon.mapM f') r)
  where
    run :: State Word a -> a
    run = flip evalState 0

    f' :: K Int x -> State Word (K Int x)
    f' (K x) = state $ \s -> first K $ f (x, s)

test_zip ::
     SomeRecordPair (K Int) (K Int)
  -> Property
test_zip r =
        onModlRecordPair pTop Modl.zip r
    === onAnonRecordPair pTop Anon.zip r

test_zipWith ::
     SomeRecordPair (K Int) (K Int)
  -> Fun (Int, Int) Int
  -> Property
test_zipWith r (applyFun -> f) =
        onModlRecordPair pTop (Modl.zipWith f') r
    === onAnonRecordPair pTop (Anon.zipWith f') r
  where
    f' :: K Int x -> K Int x -> K Int x
    f' (K x) (K y) = K $ f (x, y)

test_zipWithM ::
     SomeRecordPair (K Int) (K Int)
  -> Fun (Int, Int, Word) (Int, Word)
  -> Property
test_zipWithM r (applyFun -> f) =
        (run $ onModlRecordPairM pTop (Modl.zipWithM f') r)
    === (run $ onAnonRecordPairM pTop (Anon.zipWithM f') r)
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
        (run $ onModlRecordM pTop Modl.sequenceA r')
    === (run $ onAnonRecordM pTop Anon.sequenceA r')
  where
    run :: State Word a -> a
    run = flip evalState 0

    r' :: SomeRecord (State Word :.: K Int)
    r' = onModlRecord pTop (Modl.map f') r

    f' :: K Int x -> (State Word :.: K Int) x
    f' (K x) = Comp $ state $ \s -> first K $ f (x, s)

test_pure :: SomeFields -> Property
test_pure sf =
        someModlRecord      sf (\mf -> Modl.pure mf (K True))
    === someAnonRecord pTop sf (       Anon.pure    (K True))

test_ap ::
     SomeRecordPair (K Int) (K Int)
  -> Property
test_ap (SR2 mf rx ry) =
        onModlRecordPair pTop Modl.ap r'
    === onAnonRecordPair pTop Anon.ap r'
  where
    r' :: SomeRecordPair (K Int -.-> K Int) (K Int)
    r' = SR2 mf (Modl.map f rx) ry

    f :: K Int x -> (K Int -.-> K Int) x
    f (K x) = fn $ \(K y) -> K (x + y)

-- | Test 'Anon.distribute'
--
-- We do not have a model implementation, but instead test that
-- 'Anon.distribute' is right inverse to 'Anon.sequenceA'.
test_distribute ::
     SomeRecordPair (K Int) (K Int)
  -> Property
test_distribute = \(SR2 fields r1 r2) ->
    Modl.fieldsKnown fields $
      go fields r1 r2
  where
    go :: forall r.
         Anon.KnownFields r
      => Modl.ModelFields r
      -> Modl.ModelRecord (K Int) r
      -> Modl.ModelRecord (K Int) r
      -> Property
    go fields r1 r2 =
            rs
        === Anon.sequenceA (Anon.distribute rs)
      where
        rs :: Pair (Anon.Record (K Int) r)
        rs = MkPair (Modl.toRecord fields r1) (Modl.toRecord fields r2)
