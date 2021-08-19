{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Check that the functions on 'Rep' can be called on 'undefined'
module Test.Record.Sanity.Laziness (tests) where

import Control.Exception
import Data.List (isInfixOf)
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic
import Data.Record.TH

import qualified Data.Record.Generic.Rep          as Rep
import qualified Data.Record.Generic.Rep.Internal as Rep

import Test.Record.Util

{-------------------------------------------------------------------------------
  Example record
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
      data R = MkR {
            ri :: Word
          , rb :: Bool
          }
        deriving (Show, Eq)
    |]

example :: R
example = [lr| MkR { ri = 5, rb = True } |]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Sanity.Laziness" [
      testCase "mapWithIndex" test_mapWithIndex
    , testCase "ap"           test_ap
    , testCase "map"          test_map
    , testCase "map'"         test_map'
    , testCase "mapM"         test_mapM
    , testCase "cmap"         test_cmap
    , testCase "cmapM"        test_cmapM
    , testCase "zipWithM"     test_zipWithM
    , testCase "czipWithM"    test_czipWithM
    ]

test_mapWithIndex :: Assertion
test_mapWithIndex =
    assertEqual "" expected actual
  where
    expected, actual :: Rep (K Int) R
    expected = Rep.unsafeFromList [0, 1]
    actual   = Rep.mapWithIndex (\ix _ -> K $ Rep.indexToInt ix) undefined

test_ap :: Assertion
test_ap =
    assertEqual "" expected actual
  where
    fns :: Rep (f -.-> I) R
    fns = Rep.map' (\x -> Fn $ \_ -> x) (from example)

    expected, actual :: R
    expected = example
    actual   = to $ Rep.ap fns undefined

test_map :: Assertion
test_map =
    assertEqual "" expected actual
  where
    expected, actual :: Rep (K Int) R
    expected = Rep.unsafeFromList [0, 0]
    actual   = Rep.map (\_ -> K 0) undefined

-- Just to be sure: if we use map' instead of map, we get bottom
test_map' :: Assertion
test_map' = expectException isExpectedException $ do
    assertEqual "" expected actual
  where
    isExpectedException :: SomeException -> Bool
    isExpectedException e = "undefined" `isInfixOf` show e

    expected, actual :: Rep (K Int) R
    expected = Rep.unsafeFromList [0, 0]
    actual   = Rep.map' (\_ -> K 0) undefined

test_mapM :: Assertion
test_mapM = do
    r <- newIORef 1

    let next :: f x -> IO (K Int x)
        next _ = atomicModifyIORef r $ \i -> (i + 1, K i)

    actual :: Rep (K Int) R <- Rep.mapM next undefined
    assertEqual "" expected actual
  where
    expected :: Rep (K Int) R
    expected = Rep.unsafeFromList [1, 2]

test_cmap :: Assertion
test_cmap =
    assertEqual "" expected actual
  where
    expected, actual :: R
    expected = [lr| MkR { ri = 0, rb = False } |]
    actual   = to $ Rep.cmap (Proxy @Bounded) (\_ -> I minBound) undefined

test_cmapM :: Assertion
test_cmapM = do
    r <- newIORef False

    let next :: Bounded x => f x -> IO (I x)
        next _ = do
            b <- atomicModifyIORef r $ \b -> (not b, b)
            return . I $ if b then maxBound else minBound

    actual :: R <- to <$> Rep.cmapM (Proxy @Bounded) next undefined
    assertEqual "" expected actual
  where
    expected :: R
    expected = [lr| MkR { ri = 0, rb = True } |]

test_zipWithM :: Assertion
test_zipWithM =
    assertEqual "" expected actual
  where
    expected, actual :: Maybe (Rep (K Int) R)
    expected = Just $ Rep.unsafeFromList [0, 0]
    actual   = Rep.zipWithM (\_ _ -> Just $ K 0) undefined undefined

test_czipWithM :: Assertion
test_czipWithM =
    assertEqual "" expected actual
  where
    expected, actual :: Maybe R
    expected = Just $ [lr| MkR { ri = 0, rb = False } |]
    actual   = to <$> Rep.czipWithM
                        (Proxy @Bounded)
                        (\_ _ -> Just $ I minBound)
                        undefined
                        undefined
