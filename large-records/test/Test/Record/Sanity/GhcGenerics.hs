{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Test.Record.Sanity.GhcGenerics (tests) where

import Data.Record.Generic.GHC
import Generics.Deriving.Show (gshowsPrecdefault, GShow'(..))

import qualified GHC.Generics as GHC

import Test.Tasty
import Test.Tasty.HUnit

{-# ANN type R largeRecordStrict #-}
data R = MkR { a :: Int }
  deriving (Show)

instance GShow' (ThroughLRGenerics R) where
  gshowsPrec' _ p (WrapThroughLRGenerics x) = showsPrec p x

-- | Beam-style "force to go through GHC generics" style function
showGenerically :: (GHC.Generic a, GShow' (GHC.Rep a)) => a -> String
showGenerically x = gshowsPrecdefault 0 x ""

tests :: TestTree
tests = testGroup "Test.Record.Sanity.GhcGenerics" [
      testCase "show" test_show
    ]

test_show :: Assertion
test_show =
    assertEqual "" (show example) $ showGenerically (MkR { a = 5 })
  where
    example :: R
    example = MkR { a = 5 }