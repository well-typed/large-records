{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- Lots of fields defined here are never used directly
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Test cases in Andres' original sketch for this library
module Test.Record.Beam.Andres (tests) where

import Control.Applicative
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.Record.TH
import Database.Beam
import Database.Beam.Schema.Tables
import GHC.Records.Compat

import qualified Data.List.NonEmpty as NE
import qualified GHC.Generics       as GHC

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Beam ()

import Test.Record.Beam.Util.Orphans ()

{-------------------------------------------------------------------------------
  Table A
-------------------------------------------------------------------------------}

largeRecord defaultLazyOptions [d|
     data LRTableA (f :: Type -> Type) = MkLRTableA {
           fldA1 :: Columnar f Int
         , fldA2 :: Columnar f Int
         }
       deriving stock (Eq, Show)
       deriving anyclass (Beamable)
  |]

instance Table LRTableA where
  newtype PrimaryKey LRTableA f = LRTableAKey (Columnar f Int)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey = LRTableAKey . getField @"fldA1"

deriving instance Show (Columnar f Int) => Show (PrimaryKey LRTableA f)
deriving instance Eq   (Columnar f Int) => Eq   (PrimaryKey LRTableA f)

{-------------------------------------------------------------------------------
  Table B: has mixin (reference to table A), as well as nullable fields
-------------------------------------------------------------------------------}

largeRecord defaultLazyOptions [d|
     data LRTableB (f :: Type -> Type) = MkLRTableB {
           fldB1 :: Columnar f Int
         , fldB2 :: Columnar f Int
         , fldB3 :: Columnar f Bool
         , fldB4 :: Columnar (Nullable f) Char
         , fldB5 :: Columnar f Int
         , fldB6 :: Columnar f String
         , fldB7 :: LRTableA f
         , fldB8 :: PrimaryKey LRTableA f
         , fldB9 :: PrimaryKey LRTableA (Nullable f)
         }
       deriving stock (Eq, Show)
       deriving anyclass (Beamable)
  |]

instance Table LRTableB where
  data PrimaryKey LRTableB f = LRTableBKey (Columnar f Int)
    deriving stock (GHC.Generic)
    deriving anyclass (Beamable)

  primaryKey = LRTableBKey . getField @"fldB1"

{-------------------------------------------------------------------------------
  The database definition
-------------------------------------------------------------------------------}

-- | Example of a domain type
--
-- <https://haskell-beam.github.io/beam/user-guide/databases/#domain-types>
newtype EvenInt = EvenInt Int

largeRecord defaultLazyOptions [d|
     data LRDB (f :: Type -> Type) = MkLRDB {
           tblA   :: f (TableEntity LRTableA)
         , tblB   :: f (TableEntity LRTableB)
         , viewA  :: f (ViewEntity LRTableA)
         , domTyp :: f (DomainTypeEntity EvenInt)
         }
       deriving (Show, Eq)
  |]

instance Database be LRDB

exDbSettings :: DatabaseSettings be LRDB
exDbSettings = defaultDbSettings

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Record.Beam.Andres" [
      testCase "zipBeamFields"   test_zipBeamFields
    , testCase "tblSkeleton"     test_tblSkeleton
    , testCase "zipTables"       test_zipTables
    , testCase "autoDbSettings"  test_autoDbSettings
    , testCase "withConstraints" test_withConstraints
    ]

test_zipBeamFields :: Assertion
test_zipBeamFields =
    assertEqual "" (runIdentity (zipBeamFieldsM alt ex1 ex2)) exRes
  where
    alt :: forall a.
         Columnar' Maybe a
      -> Columnar' Maybe a
      -> Identity (Columnar' Maybe a)
    alt (Columnar' x) (Columnar' y) = Identity (Columnar' (x <|> y))

    ex1 :: LRTableB Maybe
    ex1 = MkLRTableB {
          fldB1 = Just 2
        , fldB2 = Just 2
        , fldB3 = Nothing
        , fldB4 = Just (Just 'x')
        , fldB5 = Just 4
        , fldB6 = Nothing
        , fldB7 = MkLRTableA {
                      fldA1 = Just 8
                    , fldA2 = Nothing
                    }
        , fldB8 = LRTableAKey (Just 11)
        , fldB9 = LRTableAKey Nothing
        }

    ex2 :: LRTableB Maybe
    ex2 = MkLRTableB {
          fldB1 = Nothing
        , fldB2 = Just 3
        , fldB3 = Nothing
        , fldB4 = Nothing
        , fldB5 = Nothing
        , fldB6 = Just "foo"
        , fldB7 = MkLRTableA {
                      fldA1 = Nothing
                    , fldA2 = Just 9
                    }
        , fldB8 = LRTableAKey (Just 22)
        , fldB9 = LRTableAKey (Just Nothing)
        }

    exRes :: LRTableB Maybe
    exRes = MkLRTableB {
          fldB1 = Just 2
        , fldB2 = Just 2
        , fldB3 = Nothing
        , fldB4 = Just (Just 'x')
        , fldB5 = Just 4
        , fldB6 = Just "foo"
        , fldB7 = MkLRTableA {
                      fldA1 = Just 8
                    , fldA2 = Just 9
                    }
        , fldB8 = LRTableAKey (Just 11)
           -- Outermost Maybe is our choice of @f@, innermost due to 'Nullable'
        , fldB9 = LRTableAKey (Just Nothing)
        }

test_tblSkeleton :: Assertion
test_tblSkeleton = do
    assertEqual "" tblSkeleton ex
  where
    ex :: TableSkeleton LRTableB
    ex = MkLRTableB {
          fldB1 = Ignored
        , fldB2 = Ignored
        , fldB3 = Ignored
        , fldB4 = Ignored
        , fldB5 = Ignored
        , fldB6 = Ignored
        , fldB7 = MkLRTableA {
                      fldA1 = Ignored
                    , fldA2 = Ignored
                    }
        , fldB8 = LRTableAKey Ignored
        , fldB9 = LRTableAKey Ignored
        }

test_zipTables :: Assertion
test_zipTables = do
    assertEqual "" (runIdentity (zipTables (Proxy @()) f dbX dbY)) dbRes
  where
    f :: Const Int tbl -> Const Int tbl -> Identity (Const Bool tbl)
    f (Const x) (Const y) = Identity $ Const (x > y)

    dbX :: LRDB (Const Int)
    dbX = MkLRDB {
          tblA   = Const 10
        , tblB   = Const 20
        , viewA  = Const 10
        , domTyp = Const 20
        }

    dbY :: LRDB (Const Int)
    dbY = MkLRDB {
          tblA   = Const 12
        , tblB   = Const 18
        , viewA  = Const 12
        , domTyp = Const 18
        }

    dbRes :: LRDB (Const Bool)
    dbRes = MkLRDB {
          tblA   = Const False
        , tblB   = Const True
        , viewA  = Const False
        , domTyp = Const True
        }

test_autoDbSettings :: Assertion
test_autoDbSettings =
    assertEqual "" exDbSettings settings
  where
    settings :: LRDB (DatabaseEntity be db)
    settings = MkLRDB {
        tblA = DatabaseEntity $
                 DatabaseTable {
                     dbTableSchema      = Nothing
                   , dbTableOrigName    = "tblA"
                   , dbTableCurrentName = "a"
                   , dbTableSettings    = MkLRTableA {
                         fldA1 = TableField {_fieldPath = NE.fromList ["fldA1"], _fieldName = "a1"}
                       , fldA2 = TableField {_fieldPath = NE.fromList ["fldA2"], _fieldName = "a2"}
                       }
                   }
      , tblB = DatabaseEntity $
                 DatabaseTable {
                     dbTableSchema      = Nothing
                   , dbTableOrigName    = "tblB"
                   , dbTableCurrentName = "b"
                   , dbTableSettings    = MkLRTableB {
                         fldB1 = TableField {_fieldPath = NE.fromList ["fldB1"], _fieldName = "b1"}
                       , fldB2 = TableField {_fieldPath = NE.fromList ["fldB2"], _fieldName = "b2"}
                       , fldB3 = TableField {_fieldPath = NE.fromList ["fldB3"], _fieldName = "b3"}
                       , fldB4 = TableField {_fieldPath = NE.fromList ["fldB4"], _fieldName = "b4"}
                       , fldB5 = TableField {_fieldPath = NE.fromList ["fldB5"], _fieldName = "b5"}
                       , fldB6 = TableField {_fieldPath = NE.fromList ["fldB6"], _fieldName = "b6"}
                       , fldB7 = MkLRTableA {
                               fldA1 = TableField {_fieldPath = NE.fromList ["fldB7", "fldA1"], _fieldName = "b7__a1"}
                             , fldA2 = TableField {_fieldPath = NE.fromList ["fldB7", "fldA2"], _fieldName = "b7__a2"}
                           }
                       , fldB8 = LRTableAKey (TableField {_fieldPath = NE.fromList ["fldB8", "fldA1"], _fieldName = "b8__a1"})
                       , fldB9 = LRTableAKey (TableField {_fieldPath = NE.fromList ["fldB9", "fldA1"], _fieldName = "b9__a1"})
                       }
                   }
      , viewA = DatabaseEntity $
                  DatabaseView {
                        dbViewSchema      = Nothing
                      , dbViewOrigName    = "viewA"
                      , dbViewCurrentName = "a"
                      , dbViewSettings    = MkLRTableA {
                          fldA1 = TableField {_fieldPath = NE.fromList ["fldA1"], _fieldName = "a1"}
                        , fldA2 = TableField {_fieldPath = NE.fromList ["fldA2"], _fieldName = "a2"}
                      }
                    }
      , domTyp = DatabaseEntity $
                   DatabaseDomainType Nothing "domTyp"
      }

test_withConstraints :: Assertion
test_withConstraints =
    assertEqual "" (showTable canShowB) res
  where
    canShowB :: LRTableB (WithConstraint Show)
    canShowB = withConstrainedFields ex

    ex :: LRTableB Identity
    ex = MkLRTableB {
          fldB1 = 1
        , fldB2 = 2
        , fldB3 = False
        , fldB4 = Just 'a'
        , fldB5 = 4
        , fldB6 = "b"
        , fldB7 = MkLRTableA {
                      fldA1 = 5
                    , fldA2 = 6
                    }
        , fldB8 = LRTableAKey 7
        , fldB9 = LRTableAKey (Just 8)
        }

    -- Note the use of undefined here! Beam does similar things internally;
    -- large-records has been modified so that this works.
    showTable :: Beamable tbl => tbl (WithConstraint Show) -> tbl (Const String)
    showTable tbl = runIdentity $
        zipBeamFieldsM aux tbl undefined
      where
        aux ::
             Columnar' (WithConstraint Show) a
          -> Columnar' whatever a
          -> Identity (Columnar' (Const String) a)
        aux (Columnar' (WithConstraint x)) _ = Identity $
            Columnar' (Const (show x))

    res :: LRTableB (Const String)
    res = MkLRTableB {
          fldB1 = Const "1"
        , fldB2 = Const "2"
        , fldB3 = Const "False"
        , fldB4 = Const "Just 'a'"
        , fldB5 = Const "4"
        , fldB6 = Const "\"b\""
        , fldB7 = MkLRTableA {fldA1 = Const "5", fldA2 = Const "6"}
        , fldB8 = LRTableAKey (Const "7")
        , fldB9 = LRTableAKey (Const "Just 8")
        }

