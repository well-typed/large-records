{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Beam.ZipTables (
    GZipLargeTables
  , ZipBeamFieldsI
  ) where

import Data.Kind
import Data.Proxy
import Data.Record.Beam.Internal
import Data.Record.Beam.Interpretation
import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables

import qualified Data.Record.Generic.Rep as Rep

type GZipLargeTables table f g h = (
    Generic (table f)
  , Generic (table g)
  , Generic (table h)
  , Generic (table Uninterpreted)
  , Constraints (table Uninterpreted) ZipBeamFieldsI
  , HasNormalForm (BeamInterpretation f) (table f) (table Uninterpreted)
  , HasNormalForm (BeamInterpretation g) (table g) (table Uninterpreted)
  , HasNormalForm (BeamInterpretation h) (table h) (table Uninterpreted)
  )

instance GZipLargeTables table f g h
      => GZipTables f g h exposedRep
                          (ThroughLRGenerics (table f))
                          (ThroughLRGenerics (table g))
                          (ThroughLRGenerics (table h)) where
  gZipTables _ f x y =
    fmap (WrapThroughLRGenerics . to . denormalize1 (Proxy @BeamInterpretation)) $
      Rep.czipWithM
        (Proxy @ZipBeamFieldsI)
        (zipBeamFieldsI f)
        (normalize1 (Proxy @BeamInterpretation) (from (unwrapThroughLRGenerics x)))
        (normalize1 (Proxy @BeamInterpretation) (from (unwrapThroughLRGenerics y)))

{-------------------------------------------------------------------------------
  Cases for 'gZipTables'
-------------------------------------------------------------------------------}

class ZipBeamFieldsI (a :: Type) where
  zipBeamFieldsI ::
       Applicative m
    => (forall x. Columnar' f x -> Columnar' g x -> m (Columnar' h x))
    -> Interpret (BeamInterpretation f) a
    -> Interpret (BeamInterpretation g) a
    -> m (Interpret (BeamInterpretation h) a)

instance ZipBeamFieldsI (Uninterpreted x) where
  zipBeamFieldsI f = liftInterpretedA2 $ liftColumnarA2 (Proxy @x) f

instance Beamable table => ZipBeamFieldsI (table Uninterpreted) where
  zipBeamFieldsI f = liftInterpretedA2 $ zipBeamFieldsM f

instance Beamable table => ZipBeamFieldsI (table (Nullable Uninterpreted)) where
  zipBeamFieldsI f = liftInterpretedA2 $ zipBeamFieldsM (liftNullableA2 f)


