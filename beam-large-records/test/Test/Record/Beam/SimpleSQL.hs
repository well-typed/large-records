{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor -fplugin=Data.Record.Plugin #-}

-- | Simple but complete example that does an SQL INSERT and SELECT
module Test.Record.Beam.SimpleSQL (
    tests
    -- * Exported to avoid compiler warnings
  , LargeTable(..)
  , ExampleDb(..)
  ) where

import Data.Int
import Data.Kind
import Data.Text (Text)
import Database.Beam

import qualified Database.SQLite.Simple as SQLite
import qualified GHC.Generics           as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Beam ()

import Test.Record.Beam.Util.SQLite

{-------------------------------------------------------------------------------
  Large record example
-------------------------------------------------------------------------------}

{-# ANN type LargeTable largeRecordStrict #-}
data LargeTable (f :: Type -> Type) = MkLargeTable {
      largeTableId    :: Columnar f Int32
    , largeTableField :: Columnar f Text
    }
  deriving stock (Show, Eq)
  deriving anyclass (Beamable)

large1, large2 :: LargeTable Identity
large1 = MkLargeTable 1 "hi"
large2 = MkLargeTable 2 "ho"

instance Table LargeTable where
  newtype PrimaryKey LargeTable f = LargeTableKey (Columnar f Int32)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey tbl = LargeTableKey tbl.largeTableId

{-------------------------------------------------------------------------------
  The full database
-------------------------------------------------------------------------------}

{-# ANN type ExampleDb largeRecordStrict #-}
data ExampleDb (f :: Type -> Type) = MkExampleDb {
      exampleDbLargeTable  :: f (TableEntity LargeTable)
    }
  deriving (Show)

instance Database be ExampleDb

exampleDb :: DatabaseSettings be ExampleDb
exampleDb = defaultDbSettings

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Beam.SimpleSQL" [
      testCase "insert_select" test_insert_select
    ]

test_insert_select :: Assertion
test_insert_select = runInMemory $ \conn -> do
    liftIO $ SQLite.execute_ conn $
      "CREATE TABLE db_large_table (table_id INT PRIMARY KEY NOT NULL, table_field VARCHAR NOT NULL);"

    runInsert $
      insert exampleDb.exampleDbLargeTable $ insertValues [
          large1
        , large2
        ]

    allLarge <- runSelectReturningList $ select $
      orderBy_ (\x -> asc_ (x.largeTableId)) $ all_ exampleDb.exampleDbLargeTable
    liftIO $ assertEqual "allLarge" allLarge [large1, large2]

