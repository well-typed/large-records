{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Record.Sanity.Transforms.MiniBeam (
    Nullable
  , Columnar
  , Columnar'(..)
  , Beamable(..)
  , Table(..)
  ) where

import Data.Functor.Identity
import Data.Kind

data Nullable (c :: Type -> Type) x

type family Columnar (f :: Type -> Type) x where
  Columnar Identity     x = x
  Columnar (Nullable c) x = Columnar c (Maybe x)
  Columnar f            x = f x

newtype Columnar' f a = Columnar' (Columnar f a)

class Beamable table where
  zipBeamFieldsM ::
       Applicative m
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> table f -> table g -> m (table h)

class (Beamable table, Beamable (PrimaryKey table)) => Table table where
  data PrimaryKey table (f :: Type -> Type) :: Type

  primaryKey :: table f -> PrimaryKey table f
