{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Beam.ZipDatabase (
    GZipLargeDatabase
  , ZipTablesI
  ) where

import Data.Proxy
import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables

import qualified Data.Record.Generic.Rep as Rep

type GZipLargeDatabase db f g h = (
    Generic (db f)
  , Generic (db g)
  , Generic (db h)
  , Generic (db Uninterpreted)
  , Constraints (db Uninterpreted) ZipTablesI
  , HasNormalForm (DefaultInterpretation f) (db f) (db Uninterpreted)
  , HasNormalForm (DefaultInterpretation g) (db g) (db Uninterpreted)
  , HasNormalForm (DefaultInterpretation h) (db h) (db Uninterpreted)
  )

instance GZipLargeDatabase db f g h
      => GZipDatabase be f g h (ThroughLRGenerics (db f))
                               (ThroughLRGenerics (db g))
                               (ThroughLRGenerics (db h)) where
  gZipDatabase (_, _, _, pBackend) f x y =
      fmap (WrapThroughLRGenerics . to . denormalize1 (Proxy @DefaultInterpretation)) $
        Rep.czipWithM
          (Proxy @ZipTablesI)
          (zipTablesI pBackend f)
          (normalize1 (Proxy @DefaultInterpretation) (from (unwrapThroughLRGenerics x)))
          (normalize1 (Proxy @DefaultInterpretation) (from (unwrapThroughLRGenerics y)))

{-------------------------------------------------------------------------------
  Internal: cases for 'gzipTables'
-------------------------------------------------------------------------------}

class ZipTablesI a where
  zipTablesI ::
        Applicative m
     => Proxy be
     -> (forall tbl. (IsDatabaseEntity be tbl, DatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl))
     -> Interpret (DefaultInterpretation f) a
     -> Interpret (DefaultInterpretation g) a
     -> m (Interpret (DefaultInterpretation h) a)

instance Table tbl => ZipTablesI (Uninterpreted (TableEntity tbl)) where
  zipTablesI _ f = liftInterpretedA2 f

instance Beamable tbl => ZipTablesI (Uninterpreted (ViewEntity tbl)) where
  zipTablesI _ f = liftInterpretedA2 f

instance ZipTablesI (Uninterpreted (DomainTypeEntity a)) where
  zipTablesI _ f = liftInterpretedA2 f
