{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Record.Sanity.GhcGenerics (tests) where

import Data.Function (on)
import Data.Proxy
import Data.Record.Generic.GHC
import Data.Record.TH
import Data.SOP.BasicFunctors
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Record.Generic     as LR
import qualified Data.Record.Generic.Eq  as LR
import qualified Data.Record.Generic.Rep as Rep
import qualified Generics.Deriving.Eq    as GHC
import qualified GHC.Generics            as GHC

{-------------------------------------------------------------------------------
  Example large record
-------------------------------------------------------------------------------}

largeRecord defaultPureScript [d|
      data LargeRecord = MkLargeRecord {
            largeField1 :: Int
          , largeField2 :: Bool
          }
    |]

example :: LargeRecord
example = MkLargeRecord { largeField1 = 1, largeField2 = True }

{-------------------------------------------------------------------------------
  Show that we can use geqdefault on a large record
-------------------------------------------------------------------------------}

instance ( LR.Generic a
         , LR.Constraints a Eq
         ) => GHC.GEq' (ThroughLRGenerics a) where
  geq' = LR.geq `on` unwrapThroughLRGenerics

allEqualTo :: (GHC.Generic a, GHC.GEq' (GHC.Rep a)) => a -> [a] -> Bool
allEqualTo x = all (GHC.geqdefault x)

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

simpleRecordToTable :: (GHC.Generic a, GRecordToTable (GHC.Rep a)) => a -> Table
simpleRecordToTable = Table . gRecordToTable . GHC.from

data SimpleRecord = MkSimpleRecord {
      simpleField1 :: Int
    , simpleField2 :: Bool
    }
  deriving (GHC.Generic)

-- The goal is to reuse the instance for fields
-- TODO: We could potentially extend this to the other metadata as well
largeRecordToTable :: forall a.
     (LR.Generic a, LR.Constraints a Show)
  => a -> Table
largeRecordToTable = \x ->
    Table {
        tableFields = concat . Rep.collapse $
            Rep.czipWith
              (Proxy @Show)
              aux
              (LR.from x)
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
tests = testGroup "Test.Record.Sanity.GhcGenerics" [
      testCase "allEqualTo"          test_allEqualTo
    , testCase "simpleRecordToTable" test_simpleRecordToTable
    , testCase "largeRecordToTable"  test_largeRecordToTable
    ]

test_allEqualTo :: Assertion
test_allEqualTo = assertEqual "" (allEqualTo example [example]) True

-- Just a sanity check that the standard GHC generic functions works as intended
test_simpleRecordToTable :: Assertion
test_simpleRecordToTable =
    assertEqual "" (simpleRecordToTable r) tbl
  where
    r :: SimpleRecord
    r = MkSimpleRecord 1 True

    tbl :: Table
    tbl = Table [
          ("simpleField1", "1")
        , ("simpleField2", "True")
        ]

test_largeRecordToTable :: Assertion
test_largeRecordToTable =
    assertEqual "" (largeRecordToTable example) tbl
  where
    tbl :: Table
    tbl = Table [
          ("largeField1", "1")
        , ("largeField2", "True")
        ]