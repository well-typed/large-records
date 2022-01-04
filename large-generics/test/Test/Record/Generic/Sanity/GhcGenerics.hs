{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Record.Generic.Sanity.GhcGenerics (tests) where

import Data.Function (on)
import Data.Proxy
import Data.SOP.BasicFunctors
import Generics.Deriving.Eq (GEq'(..), geqdefault)

import qualified GHC.Generics as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Generic.GHC

import qualified Data.Record.Generic     as L
import qualified Data.Record.Generic.Eq  as L (geq)
import qualified Data.Record.Generic.Rep as Rep

import Test.Record.Generic.Infra.Examples

{-------------------------------------------------------------------------------
  Show that we can use geqdefault on a large record
-------------------------------------------------------------------------------}

instance ( L.Generic a
         , L.Constraints a Eq
         ) => GEq' (ThroughLRGenerics a) where
  geq' = L.geq `on` unwrapThroughLRGenerics

allEqualTo :: (GHC.Generic a, GEq' (GHC.Rep a)) => a -> [a] -> Bool
allEqualTo x = all (geqdefault x)

{-------------------------------------------------------------------------------
  Example with GHC field metadata
-------------------------------------------------------------------------------}

class GRecordToTable f where
  gRecordToTable :: f p -> [(String, String)]

instance GRecordToTable f
      => GRecordToTable (GHC.M1 GHC.D c f) where
  gRecordToTable (GHC.M1 x) = gRecordToTable x

instance GRecordToTable f
      => GRecordToTable (GHC.M1 GHC.C c f) where
  gRecordToTable (GHC.M1 x) = gRecordToTable x

instance (GRecordToTable f, GRecordToTable g)
      => GRecordToTable (f GHC.:*: g) where
  gRecordToTable (l GHC.:*: r) = gRecordToTable l ++ gRecordToTable r

instance (GHC.Selector f, Show a)
      => GRecordToTable (GHC.M1 GHC.S f (GHC.K1 GHC.R a)) where
  gRecordToTable f@(GHC.M1 (GHC.K1 x)) = [(GHC.selName f, show x)]

data Table = Table {
      tableFields :: [(String, String)]
    }
  deriving (Show, Eq)

ghcRecordToTable :: (GHC.Generic a, GRecordToTable (GHC.Rep a)) => a -> Table
ghcRecordToTable = Table . gRecordToTable . GHC.from

-- The goal is to reuse the instance for fields
-- TODO: We could potentially extend this to the other metadata as well
largeRecordToTable :: forall a.
     (L.Generic a, L.Constraints a Show)
  => a -> Table
largeRecordToTable = \x ->
    Table {
        tableFields = concat . Rep.collapse $
            Rep.czipWith
              (Proxy @Show)
              aux
              (L.from x)
              (ghcMetadataFields (ghcMetadata (Proxy @a)))
      }
  where
    aux :: Show x => I x -> GhcFieldMetadata x -> K [(String, String)] x
    aux (I x) (GhcFieldMetadata p) = K $ gRecordToTable $ aux' x p

    aux' :: x -> Proxy f -> GHC.M1 GHC.S f (GHC.K1 GHC.R x) p
    aux' x _ = GHC.M1 (GHC.K1 x)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Generic.Sanity.GhcGenerics" [
      testCase "allEqualTo"          test_allEqualTo
    , testCase "simpleRecordToTable" test_simpleRecordToTable
    , testCase "largeRecordToTable"  test_largeRecordToTable
    ]

test_allEqualTo :: Assertion
test_allEqualTo =
    assertEqual "" True $
      allEqualTo exampleSimpleRecord [exampleSimpleRecord]

-- Just a sanity check that the standard GHC generic functions works as intended
test_simpleRecordToTable :: Assertion
test_simpleRecordToTable =
    assertEqual "" expectedTable $
      ghcRecordToTable exampleSimpleRecord

test_largeRecordToTable :: Assertion
test_largeRecordToTable =
    assertEqual "" expectedTable $
      largeRecordToTable exampleSimpleRecord

expectedTable :: Table
expectedTable = Table [
      ("simpleRecordField1", "5")
    , ("simpleRecordField2", "True")
    ]

