{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Standard Haskell records with a hand-written Generic instance
--
-- @large-records@ and @large-anon@ provide different record representations,
-- but here we want to test the generics infrastructure independent from these
-- libraries. The definitions here are simple, not intended to check for core
-- size; we do that in large-records and large-anon.
--
-- These definitions serve as a simple example of how the 'Generic' class might
-- be instantiated, and are used as a basis for testing.
module Test.Record.Generic.Infra.Examples (
    -- * Simple
    SimpleRecord(..)
  , exampleSimpleRecord
    -- * With type parameters
  , ParamRecord(..)
  , exampleParamRecord
    -- * Higher-kinded
  , Regular(..)
  , Irregular(..)
  , MultiFun(..)
  , exampleRegular
  , exampleIrregular
    -- * Beam-like
  , MixinTable(..)
  , FullTable(..)
  , PrimaryKey(..)
  , exampleMixinTable
  ) where

import Data.Kind

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Test.QuickCheck

import Data.Record.Generic
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)

import qualified Data.Record.Generic.Rep.Internal as Rep

import Test.Record.Generic.Infra.Beam.Mini
import Data.Functor.Identity

{-------------------------------------------------------------------------------
  Simple record
-------------------------------------------------------------------------------}

data SimpleRecord = MkSimpleRecord {
      simpleRecordField1 :: Word
    , simpleRecordField2 :: Bool
    }
  deriving (Show, Eq, GHC.Generic) -- GHC generics for the GhcGenerics tests

exampleSimpleRecord :: SimpleRecord
exampleSimpleRecord = MkSimpleRecord 5 True

class    (c Word, c Bool) => ConstraintsSimpleRecord c where
instance (c Word, c Bool) => ConstraintsSimpleRecord c where

instance Generic SimpleRecord where
  type Constraints SimpleRecord = ConstraintsSimpleRecord
  type MetadataOf  SimpleRecord = '[ '("simpleRecordField1", Word)
                                   , '("simpleRecordField2", Bool)
                                   ]

  from MkSimpleRecord{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo simpleRecordField1
      , I $ noInlineUnsafeCo simpleRecordField2
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> simpleRecordField1)
          , I (noInlineUnsafeCo -> simpleRecordField2)
          ] = Rep.toListAny rep }
    in MkSimpleRecord{..}

  dict :: forall c.
       ConstraintsSimpleRecord c
    => Proxy c -> Rep (Dict c) SimpleRecord
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c Word)
      , noInlineUnsafeCo (Dict :: Dict c Bool)
      ]

  metadata _ = Metadata {
        recordName          = "SimpleRecord"
      , recordConstructor   = "MkSimpleRecord"
      , recordSize          = 2
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"simpleRecordField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"simpleRecordField2") FieldLazy
          ]
      }

instance Arbitrary SimpleRecord where
  arbitrary = MkSimpleRecord <$> arbitrary <*> arbitrary
  shrink (MkSimpleRecord f1 f2) = concat [
        [ MkSimpleRecord f1' f2
        | f1' <- shrink f1
        ]
      , [ MkSimpleRecord f1 f2'
        | f2' <- shrink f2
        ]
      ]

{-------------------------------------------------------------------------------
  Record with some type arguments (but not higher-kinded)
-------------------------------------------------------------------------------}

data ParamRecord a b = MkParamRecord {
      paramRecordField1 :: Word
    , paramRecordField2 :: Bool
    , paramRecordField3 :: Char
    , paramRecordField4 :: a
    , paramRecordField5 :: [b]
    }
  deriving (Eq, Ord, Show, GHC.Generic)

instance SOP.Generic (ParamRecord a b)
  -- For comparison purposes only

class    (c Word, c Bool, c Char, c a, c [b]) => ConstraintsParamRecord a b c
instance (c Word, c Bool, c Char, c a, c [b]) => ConstraintsParamRecord a b c

instance Generic (ParamRecord a b) where
  type Constraints (ParamRecord a b) = ConstraintsParamRecord a b
  type MetadataOf  (ParamRecord a b) = '[ '("paramRecordField1", Word)
                                        , '("paramRecordField2", Bool)
                                        , '("paramRecordField3", Char)
                                        , '("paramRecordField4", a)
                                        , '("paramRecordField5", [b])
                                        ]

  from MkParamRecord{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo paramRecordField1
      , I $ noInlineUnsafeCo paramRecordField2
      , I $ noInlineUnsafeCo paramRecordField3
      , I $ noInlineUnsafeCo paramRecordField4
      , I $ noInlineUnsafeCo paramRecordField5
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> paramRecordField1)
          , I (noInlineUnsafeCo -> paramRecordField2)
          , I (noInlineUnsafeCo -> paramRecordField3)
          , I (noInlineUnsafeCo -> paramRecordField4)
          , I (noInlineUnsafeCo -> paramRecordField5)
          ] = Rep.toListAny rep }
    in MkParamRecord{..}

  dict :: forall c.
       ConstraintsParamRecord a b c
    => Proxy c -> Rep (Dict c) (ParamRecord a b)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c Word)
      , noInlineUnsafeCo (Dict :: Dict c Bool)
      , noInlineUnsafeCo (Dict :: Dict c Char)
      , noInlineUnsafeCo (Dict :: Dict c a)
      , noInlineUnsafeCo (Dict :: Dict c [b])
      ]

  metadata _ = Metadata {
        recordName          = "ParamRecord"
      , recordConstructor   = "MkParamRecord"
      , recordSize          = 5
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"paramRecordField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"paramRecordField2") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"paramRecordField3") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"paramRecordField4") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"paramRecordField5") FieldLazy
          ]
      }

exampleParamRecord :: ParamRecord () Float
exampleParamRecord = MkParamRecord 5 True 'c' () [3.14]

{-------------------------------------------------------------------------------
  Higher kinded record: regular
-------------------------------------------------------------------------------}

-- | Regular example: all fields have an @f@ parameter
data Regular f = MkRegular {
        regularField1 :: f Int
      , regularField2 :: f Bool
      , regularField3 :: f String
      }

exampleRegular :: Regular I
exampleRegular = MkRegular {
      regularField1 = I 5
    , regularField2 = I True
    , regularField3 = I "a"
    }

deriving instance (Show (f Int), Show (f Bool), Show (f String)) => Show (Regular f)
deriving instance (Eq   (f Int), Eq   (f Bool), Eq   (f String)) => Eq   (Regular f)

class    (c (f Int), c (f Bool), c (f String)) => ConstraintsRegular f c
instance (c (f Int), c (f Bool), c (f String)) => ConstraintsRegular f c

instance Generic (Regular f) where
  type Constraints (Regular f) = ConstraintsRegular f
  type MetadataOf  (Regular f) = '[ '("regularField1", f Int)
                                  , '("regularField1", f Bool)
                                  , '("regularField3", f String)
                                  ]


  from MkRegular{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo regularField1
      , I $ noInlineUnsafeCo regularField2
      , I $ noInlineUnsafeCo regularField3
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> regularField1)
          , I (noInlineUnsafeCo -> regularField2)
          , I (noInlineUnsafeCo -> regularField3)
          ] = Rep.toListAny rep }
    in MkRegular{..}

  dict :: forall c.
       ConstraintsRegular f c
    => Proxy c -> Rep (Dict c) (Regular f)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c (f Int))
      , noInlineUnsafeCo (Dict :: Dict c (f Bool))
      , noInlineUnsafeCo (Dict :: Dict c (f String))
      ]

  metadata _ = Metadata {
        recordName          = "Regular"
      , recordConstructor   = "MkRegular"
      , recordSize          = 3
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"regularField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"regularField2") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"regularField3") FieldLazy
          ]
      }

{-------------------------------------------------------------------------------
  Higher kinded record: irregular
-------------------------------------------------------------------------------}

-- | Irregular example: not all fields have an @f@ parameter
data Irregular f = MkIrregular {
        irregularField1 :: f Int
      , irregularField2 :: f Bool
      , irregularField3 :: String
      }

exampleIrregular :: Irregular I
exampleIrregular = MkIrregular {
      irregularField1 = I 1234
    , irregularField2 = I True
    , irregularField3 = "hi"
    }

deriving instance (Show (f Int), Show (f Bool)) => Show (Irregular f)
deriving instance (Eq   (f Int), Eq   (f Bool)) => Eq   (Irregular f)

class    (c (f Int), c (f Bool), c String) => ConstraintsIrregular f c
instance (c (f Int), c (f Bool), c String) => ConstraintsIrregular f c

instance Generic (Irregular f) where
  type Constraints (Irregular f) = ConstraintsIrregular f
  type MetadataOf  (Irregular f) = '[ '("irregularField1", f Int)
                                    , '("irregularField1", f Bool)
                                    , '("irregularField3", String)
                                    ]

  from MkIrregular{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo irregularField1
      , I $ noInlineUnsafeCo irregularField2
      , I $ noInlineUnsafeCo irregularField3
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> irregularField1)
          , I (noInlineUnsafeCo -> irregularField2)
          , I (noInlineUnsafeCo -> irregularField3)
          ] = Rep.toListAny rep }
    in MkIrregular{..}

  dict :: forall c.
       ConstraintsIrregular f c
    => Proxy c -> Rep (Dict c) (Irregular f)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c (f Int))
      , noInlineUnsafeCo (Dict :: Dict c (f Bool))
      , noInlineUnsafeCo (Dict :: Dict c String)
      ]

  metadata _ = Metadata {
        recordName          = "Irregular"
      , recordConstructor   = "MkIrregular"
      , recordSize          = 3
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"irregularField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"irregularField2") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"irregularField3") FieldLazy
          ]
      }

{-------------------------------------------------------------------------------
  Higher kinded record: multiple functors
-------------------------------------------------------------------------------}

data MultiFun f g = MkMultiFun {
        multiFunField1 :: f Int
      , multiFunField2 :: g Bool
      , multiFunField3 :: String
      }

deriving instance (Show (f Int), Show (g Bool)) => Show (MultiFun f g)
deriving instance (Eq   (f Int), Eq   (g Bool)) => Eq   (MultiFun f g)

class    (c (f Int), c (g Bool), c String) => ConstraintsMultiFun f g c
instance (c (f Int), c (g Bool), c String) => ConstraintsMultiFun f g c

instance Generic (MultiFun f g) where
  type Constraints (MultiFun f g) = ConstraintsMultiFun f g
  type MetadataOf  (MultiFun f g) = '[ '("multiFunField1", f Int)
                                     , '("multiFunField1", g Bool)
                                     , '("multiFunField3", String)
                                     ]

  from MkMultiFun{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo multiFunField1
      , I $ noInlineUnsafeCo multiFunField2
      , I $ noInlineUnsafeCo multiFunField3
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> multiFunField1)
          , I (noInlineUnsafeCo -> multiFunField2)
          , I (noInlineUnsafeCo -> multiFunField3)
          ] = Rep.toListAny rep }
    in MkMultiFun{..}

  dict :: forall c.
       ConstraintsMultiFun f g c
    => Proxy c -> Rep (Dict c) (MultiFun f g)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c (f Int))
      , noInlineUnsafeCo (Dict :: Dict c (g Bool))
      , noInlineUnsafeCo (Dict :: Dict c String)
      ]

  metadata _ = Metadata {
        recordName          = "MultiFun"
      , recordConstructor   = "MkMultiFun"
      , recordSize          = 3
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"multiFunField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"multiFunField2") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"multiFunField3") FieldLazy
          ]
      }

{-------------------------------------------------------------------------------
  Beam-like mixin table

  This is the simpler case, because it contains only Columnar fields.
-------------------------------------------------------------------------------}

data MixinTable (f :: Type -> Type) = MkMixinTable {
      mixinTableField1 :: Columnar f Char
    , mixinTableField2 :: Columnar f Double
    }

exampleMixinTable :: MixinTable Identity
exampleMixinTable = MkMixinTable {
      mixinTableField1 = 'a'
    , mixinTableField2 = 3.14
    }

deriving instance ( Show (Columnar f Char)
                  , Show (Columnar f Double)
                  ) => Show (MixinTable f)
deriving instance ( Eq (Columnar f Char)
                  , Eq (Columnar f Double)
                  ) => Eq (MixinTable f)

class    (c (Columnar f Char), c (Columnar f Double)) => ConstraintsMixinTable f c
instance (c (Columnar f Char), c (Columnar f Double)) => ConstraintsMixinTable f c

instance Generic (MixinTable f) where
  type Constraints (MixinTable f) = ConstraintsMixinTable f
  type MetadataOf  (MixinTable f) = '[ '("mixinTableField1", Columnar f Char)
                                     , '("mixinTableField2", Columnar f Double)
                                     ]

  from MkMixinTable{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo mixinTableField1
      , I $ noInlineUnsafeCo mixinTableField2
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> mixinTableField1)
          , I (noInlineUnsafeCo -> mixinTableField2)
          ] = Rep.toListAny rep }
    in MkMixinTable{..}

  dict :: forall c.
       ConstraintsMixinTable f c
    => Proxy c -> Rep (Dict c) (MixinTable f)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c (Columnar f Char))
      , noInlineUnsafeCo (Dict :: Dict c (Columnar f Double))
      ]

  metadata _ = Metadata {
        recordName          = "MixinTable"
      , recordConstructor   = "MkMixinTable"
      , recordSize          = 2
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"mixinTableField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"mixinTableField2") FieldLazy
          ]
      }

{-------------------------------------------------------------------------------
  Beam-like full table example
-------------------------------------------------------------------------------}

data FullTable (f :: Type -> Type) = MkFullTable {
      fullTableField1 :: PrimaryKey FullTable f
    , fullTableField2 :: Columnar f Bool
    , fullTableField3 :: MixinTable f
    }

data instance PrimaryKey FullTable f = PrimA (Columnar f Int)

deriving instance Show (Columnar f Int) => Show (PrimaryKey FullTable f)
deriving instance Eq   (Columnar f Int) => Eq   (PrimaryKey FullTable f)

deriving instance ( Show (Columnar f Int)
                  , Show (Columnar f Bool)
                  , Show (Columnar f Char)
                  , Show (Columnar f Double)
                  ) => Show (FullTable f)
deriving instance ( Eq (Columnar f Int)
                  , Eq (Columnar f Bool)
                  , Eq (Columnar f Char)
                  , Eq (Columnar f Double)
                  ) => Eq (FullTable f)

class    ( c (PrimaryKey FullTable f)
         , c (Columnar f Bool)
         , c (MixinTable f)
         ) => ConstraintsFullTable f c
instance ( c (PrimaryKey FullTable f)
         , c (Columnar f Bool)
         , c (MixinTable f)
         ) => ConstraintsFullTable f c

instance Generic (FullTable f) where
  type Constraints (FullTable f) = ConstraintsFullTable f
  type MetadataOf  (FullTable f) = '[ '("fullTableField1", PrimaryKey FullTable f)
                                    , '("fullTableField2", Columnar f Bool)
                                    , '("fullTableField3", MixinTable f)
                                    ]

  from MkFullTable{..} = Rep.unsafeFromListAny [
        I $ noInlineUnsafeCo fullTableField1
      , I $ noInlineUnsafeCo fullTableField2
      , I $ noInlineUnsafeCo fullTableField3
      ]

  to rep =
    let { [ I (noInlineUnsafeCo -> fullTableField1)
          , I (noInlineUnsafeCo -> fullTableField2)
          , I (noInlineUnsafeCo -> fullTableField3)
          ] = Rep.toListAny rep }
    in MkFullTable{..}

  dict :: forall c.
       ConstraintsFullTable f c
    => Proxy c -> Rep (Dict c) (FullTable f)
  dict _ = Rep.unsafeFromListAny [
        noInlineUnsafeCo (Dict :: Dict c (PrimaryKey FullTable f))
      , noInlineUnsafeCo (Dict :: Dict c (Columnar f Bool))
      , noInlineUnsafeCo (Dict :: Dict c (MixinTable f))
      ]

  metadata _ = Metadata {
        recordName          = "FullTable"
      , recordConstructor   = "MkFullTable"
      , recordSize          = 3
      , recordFieldMetadata = Rep.unsafeFromListAny [
            noInlineUnsafeCo $
              FieldMetadata (Proxy @"fullTableField1") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"fullTableField2") FieldLazy
          , noInlineUnsafeCo $
              FieldMetadata (Proxy @"fullTableField3") FieldLazy
          ]
      }
