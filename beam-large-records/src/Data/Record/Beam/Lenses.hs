{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Beam.Lenses (
    GLargeTableLenses
  , TableLensesI
  , GLargeDatabaseLenses
  ) where

import Data.Proxy
import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Lens.VL
import Data.Record.Generic.Transform
import Database.Beam.Schema
import Lens.Micro hiding (to)

import qualified Data.Record.Generic.Rep as Rep
import qualified GHC.Generics            as GHC

import Data.Record.Beam.Interpretation

{-------------------------------------------------------------------------------
  Table lenses
-------------------------------------------------------------------------------}

type GLargeTableLenses outer tbl f = (
    Generic (tbl (Lenses outer f))
  , Generic (tbl Uninterpreted)
  , Generic (tbl f)
  , HasNormalForm (BeamInterpretation (Lenses outer f)) (tbl (Lenses outer f)) (tbl Uninterpreted)
  , HasNormalForm (BeamInterpretation f) (tbl f) (tbl Uninterpreted)
  , Constraints (tbl Uninterpreted) (TableLensesI outer f)
  )

instance GLargeTableLenses outer tbl f
      => GTableLenses outer f (ThroughLRGenerics (tbl f))
                              (ThroughLRGenerics (tbl (Lenses outer f))) where
  gTableLenses ::
       Proxy (ThroughLRGenerics (tbl f))
    -> Lens' (outer f) (ThroughLRGenerics (tbl f) p)
    -> ThroughLRGenerics (tbl (Lenses outer f)) ()
  gTableLenses _ lensToHere = WrapThroughLRGenerics $
      to . denormalize1 (Proxy @BeamInterpretation) $
        Rep.cmap
          (Proxy @(TableLensesI outer f))
          aux
          (lensesForHKRecord (Proxy @BeamInterpretation))
    where
      aux ::
           TableLensesI outer f x
        => HKRecordLens BeamInterpretation f tbl x
        -> Interpret (BeamInterpretation (Lenses outer f)) x
      aux (HKRecordLens l) = tableLensesI $
            lensToHere
          . unwrapThroughLRGenericsLens
          . l
          . interpretedLens

class TableLensesI outer f x where
  tableLensesI ::
       Lens' (outer f) (Interpreted (BeamInterpretation f) x)
    -> Interpret (BeamInterpretation (Lenses outer f)) x

-- GHC.Generic required by 'LensFor' constructor
instance GHC.Generic (outer f) => TableLensesI outer f (Uninterpreted x) where
  tableLensesI lensToHere = Interpret $ LensFor lensToHere

-- We to through the GHC generics instance so that it works both when the
-- subsubtable uses GHC generics (typically, primary keys) and when it uses
-- LR generics (typically, mixins).
instance ( GHC.Generic (sub (Lenses outer f))
         , GHC.Generic (sub f)
         , GTableLenses outer f (GHC.Rep (sub f)) (GHC.Rep (sub (Lenses outer f)))
         )
      => TableLensesI outer f (sub Uninterpreted) where
  tableLensesI lensToHere = Interpret $
      GHC.to $ gTableLenses Proxy (lensToHere . ghcGenericLens)

instance ( GHC.Generic (sub (Nullable (Lenses outer f)))
         , GHC.Generic (sub (Nullable f))
         , GTableLenses outer f (GHC.Rep (sub (Nullable f))) (GHC.Rep (sub (Nullable (Lenses outer f))))
         )
      => TableLensesI outer f (sub (Nullable Uninterpreted)) where
  tableLensesI lensToHere = Interpret $
      GHC.to $ gTableLenses Proxy (lensToHere . ghcGenericLens)

{-------------------------------------------------------------------------------
  DB lenses
-------------------------------------------------------------------------------}

type GLargeDatabaseLenses outer db f = (
    Generic (db (TableLens f outer))
  , Generic (db Uninterpreted)
  , Generic (db f)
  , HasNormalForm (DefaultInterpretation (TableLens f outer)) (db (TableLens f outer)) (db Uninterpreted)
  , HasNormalForm (DefaultInterpretation f) (db f) (db Uninterpreted)
  , Constraints (db Uninterpreted) (DbLensesI outer f)
  )

instance GLargeDatabaseLenses outer db f
      => GDatabaseLenses (outer f) (ThroughLRGenerics (db f))
                                   (ThroughLRGenerics (db (TableLens f outer))) where
  gDatabaseLenses ::
       Lens' (outer f) (ThroughLRGenerics (db f) p)
    -> ThroughLRGenerics (db (TableLens f outer)) ()
  gDatabaseLenses lensToHere = WrapThroughLRGenerics $
      to . denormalize1 (Proxy @(DefaultInterpretation)) $
        Rep.cmap
          (Proxy @(DbLensesI outer f))
          aux
          (lensesForHKRecord (Proxy @DefaultInterpretation))
    where
      aux ::
           DbLensesI outer f x
        => HKRecordLens DefaultInterpretation f db x
        -> Interpret (DefaultInterpretation (TableLens f outer)) x
      aux (HKRecordLens l) = dbLensesI $
            lensToHere
          . unwrapThroughLRGenericsLens
          . l
          . interpretedLens

class DbLensesI outer f x where
  dbLensesI ::
       Lens' (outer f) (Interpreted (DefaultInterpretation f) x)
    -> Interpret (DefaultInterpretation (TableLens f outer)) x

instance DbLensesI db f (Uninterpreted (TableEntity tbl)) where
  dbLensesI lensToHere = Interpret $ TableLens lensToHere

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ghcGenericLens :: GHC.Generic a => Lens' a (GHC.Rep a p)
ghcGenericLens f x = GHC.to <$> f (GHC.from x)

unwrapThroughLRGenericsLens :: Lens' (ThroughLRGenerics a p) a
unwrapThroughLRGenericsLens f x =
    WrapThroughLRGenerics <$> f (unwrapThroughLRGenerics x)