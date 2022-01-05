{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Beam.Constraints (
    GLargeFieldsFulfillConstraint
  , WithConstrainedFieldsI
  ) where

import Data.Record.Generic
import Data.Record.Generic.GHC
import Data.Record.Generic.Transform
import Database.Beam.Schema.Tables
import GHC.Generics hiding (Generic(..), (:.:))

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Beam.Interpretation

type GLargeFieldsFulfillConstraint tbl c = (
    Generic (tbl (HasConstraint c))
  , Generic (tbl Uninterpreted)
  , HasNormalForm (BeamInterpretation (HasConstraint c)) (tbl (HasConstraint c)) (tbl Uninterpreted)
  , Constraints (tbl Uninterpreted) (WithConstrainedFieldsI c)
  )

instance GLargeFieldsFulfillConstraint tbl c
      => GFieldsFulfillConstraint c (ThroughLRGenerics (tbl Exposed))
                                    (ThroughLRGenerics (tbl (HasConstraint c))) where
  gWithConstrainedFields pc _ = WrapThroughLRGenerics $
      to . denormalize1 (Proxy @BeamInterpretation) $
        Rep.cpure (Proxy @(WithConstrainedFieldsI c)) (withConstrainedFieldsI pc)

class WithConstrainedFieldsI c x where
  withConstrainedFieldsI :: Proxy c -> Interpret (BeamInterpretation (HasConstraint c)) x

instance c x => WithConstrainedFieldsI c (Uninterpreted x) where
  withConstrainedFieldsI pc = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (HasConstraint c x) ()
      fromBeam = gWithConstrainedFields pc (Proxy @(K1 R (Exposed x)))

instance FieldsFulfillConstraint c tbl
      => WithConstrainedFieldsI c (tbl Uninterpreted) where
  withConstrainedFieldsI pc = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (tbl (HasConstraint c)) ()
      fromBeam = gWithConstrainedFields pc (Proxy @(K1 R (tbl Exposed)))

instance FieldsFulfillConstraintNullable c tbl
      => WithConstrainedFieldsI c (tbl (Nullable Uninterpreted)) where
  withConstrainedFieldsI pc = Interpret $ unK1 fromBeam
    where
      fromBeam :: K1 R (tbl (Nullable (HasConstraint c))) ()
      fromBeam = gWithConstrainedFields pc (Proxy @(K1 R (tbl (Nullable Exposed))))
