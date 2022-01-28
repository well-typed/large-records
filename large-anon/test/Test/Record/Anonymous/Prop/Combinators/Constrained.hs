{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module Test.Record.Anonymous.Prop.Combinators.Constrained (tests) where

import Control.Monad.ST
import Data.Proxy
import Data.Record.Generic.LowerBound
import Data.SOP.BasicFunctors
import Data.STRef

import qualified Data.Record.Anonymous.Advanced as Anon

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Record.Anonymous.Prop.Model.Orphans ()
import Test.Record.Anonymous.Prop.Model.Generator

import qualified Test.Record.Anonymous.Prop.Model as Modl

tests :: TestTree
tests = testGroup "Test.Record.Anonymous.Prop.Combinators.Constrained" [
      testProperty "cpure"     test_cpure
    , testProperty "cmap"      test_cmap
    , testProperty "cmapM"     test_cmapM
    , testProperty "czipWith"  test_czipWith
    , testProperty "czipWithM" test_czipWithM
    ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

test_cpure :: SomeFields -> Property
test_cpure sf =
        someModlRecord   sf (\mf -> Modl.cpure p mf (I lowerBound))
    === someAnonRecord p sf (       Anon.cpure p    (I lowerBound))
  where
    p = Proxy @LowerBound

test_cmap :: SomeRecord I -> Property
test_cmap r =
        onModlRecord p (Modl.cmap p f) r
    === onAnonRecord p (Anon.cmap p f) r
  where
    p = Proxy @Show

    f :: Show a => I a -> K String a
    f (I x) = K (show x)

test_cmapM :: SomeRecord I -> Property
test_cmapM r =
        (run $ \ref -> onModlRecordM p (Modl.cmapM p (f ref)) r)
    === (run $ \ref -> onAnonRecordM p (Anon.cmapM p (f ref)) r)
  where
    p = Proxy @Show

    run :: (forall s. STRef s Int -> ST s a) -> (a, Int)
    run st = runST $ do
        ref <- newSTRef 0
        a   <- st ref
        (a,) <$> readSTRef ref

    f :: Show a => STRef s Int -> I a -> ST s (K String a)
    f ref (I x) = do
        modifySTRef ref succ
        return (K $ show x)

test_czipWith :: SomeRecordPair I I -> Property
test_czipWith r =
        onModlRecordPair p (Modl.czipWith p f) r
    === onAnonRecordPair p (Anon.czipWith p f) r
  where
    p = Proxy @Show

    f :: Show x => I x -> I x -> K String x
    f (I x) (I y) = K (show x ++ show y)

test_czipWithM :: SomeRecordPair I I -> Property
test_czipWithM r =
        run (\ref -> onModlRecordPairM p (Modl.czipWithM p (f ref)) r)
    === run (\ref -> onAnonRecordPairM p (Anon.czipWithM p (f ref)) r)
  where
    p = Proxy @Show

    run :: (forall s. STRef s Bool -> ST s a) -> a
    run st = runST $ newSTRef False >>= st

    -- Function that uses some state from processing the /previous/ element
    -- (As an example of an order dependency)
    f :: Show x => STRef s Bool -> I x -> I x -> ST s (I x)
    f ref (I x) (I y) = do
        b <- readSTRef ref
        writeSTRef ref $ even (length (show x))
        return . I $ if b then x else y
