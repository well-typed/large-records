{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Integration of large-generics with mini-beam
--
-- See the @beam-large-package@ for full beam integration.
module Test.Record.Generic.Infra.Beam.Interpretation (
    BeamInterpretation
  , ZipInterpreted(..)
  , gzipBeam
  ) where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy

import Data.Record.Generic
import Data.Record.Generic.Transform
import Data.Record.Generic.Lens.VL

import qualified Data.Record.Generic.Rep as Rep

import Test.Record.Generic.Infra.Beam.Mini

data BeamInterpretation (f :: Type -> Type)

type instance Interpreted (BeamInterpretation f) (table Uninterpreted) = table f
type instance Interpreted (BeamInterpretation f) (Uninterpreted x)     = Columnar f x

instance StandardInterpretation BeamInterpretation (RegularRecordLens tbl f)
instance StandardInterpretation BeamInterpretation Identity

class ZipInterpreted a where
  zipInterpreted ::
       Applicative m
    => (forall x. Columnar' f x -> Columnar' g x -> m (Columnar' h x))
    -> Interpret (BeamInterpretation f) a
    -> Interpret (BeamInterpretation g) a
    -> m (Interpret (BeamInterpretation h) a)

instance Beamable table => ZipInterpreted (table Uninterpreted) where
  zipInterpreted f = liftInterpretedA2 $ zipBeamFieldsM f

instance ZipInterpreted (Uninterpreted x) where
  zipInterpreted f = liftInterpretedA2 $ applyColumnar' (Proxy @x) f

gzipBeam :: forall m table f g h.
     ( Applicative m
     , Generic (table f)
     , Generic (table g)
     , Generic (table h)
     , Generic (table Uninterpreted)
     , Constraints (table Uninterpreted) ZipInterpreted
     , HasNormalForm (BeamInterpretation f) (table f) (table Uninterpreted)
     , HasNormalForm (BeamInterpretation g) (table g) (table Uninterpreted)
     , HasNormalForm (BeamInterpretation h) (table h) (table Uninterpreted)
     )
  => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
  -> table f -> table g -> m (table h)
gzipBeam f a b =
    fmap (to . denormalize1 (Proxy @BeamInterpretation)) $
      Rep.czipWithM
        (Proxy @ZipInterpreted)
        (zipInterpreted f)
        (normalize1 (Proxy @BeamInterpretation) (from a))
        (normalize1 (Proxy @BeamInterpretation) (from b))

