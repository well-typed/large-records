{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Data.Record.Beam.Interpretation (
    BeamInterpretation
  ) where

import Data.Kind
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables

-- | Interpretation domain
--
-- This is (mostly) for internal use; client code will probably not have to
-- interact with this directly.
--
-- See "Data.Record.Generic.Transform" for details on interpretation domains.
data BeamInterpretation (f :: Type -> Type)

type instance Interpreted (BeamInterpretation f) (Uninterpreted x)                = Columnar f x
type instance Interpreted (BeamInterpretation f) (table Uninterpreted)            = table f
type instance Interpreted (BeamInterpretation f) (table (Nullable Uninterpreted)) = table (Nullable f)
