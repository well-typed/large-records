{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: cleanup

module Test.Record.Generic.Infra.Beam.Mini (
    Columnar
  , Columnar'(..)
  , applyColumnar'
  , PrimaryKey
  , Table
  , Lenses
  , Beamable(..)
  , WrapLens(..)
  ) where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Lens.Micro (Lens')

data Nullable (c :: Type -> Type) x

data Lenses (tbl :: (Type -> Type) -> Type) (f :: Type -> Type) (x :: Type)

data WrapLens a b = WrapLens (Lens' a b)

type family Columnar (f :: Type -> Type) x where
  Columnar Identity       x = x
  Columnar (Nullable c)   x = Columnar c (Maybe x)
  Columnar (Lenses tbl f) x = WrapLens (tbl f) (Columnar f x)
  Columnar f              x = f x

newtype Columnar' f a = Columnar' { getColumnar' :: Columnar f a }

applyColumnar' :: forall m f g h x.
     Functor m
  => Proxy x
  -> (Columnar' f x -> Columnar' g x -> m (Columnar' h x))
  -> (Columnar  f x -> Columnar  g x -> m (Columnar  h x))
applyColumnar' _ f fx gx = getColumnar' <$> f (Columnar' fx) (Columnar' gx)

class Beamable (table :: (Type -> Type) -> Type) where
  zipBeamFieldsM ::
       Applicative m
    => (forall a. Columnar' f a -> Columnar' g a -> m (Columnar' h a))
    -> table f -> table g -> m (table h)

-- | Primary key of a table
--
-- In beam this is an associated type of the 'Table' class; we split this off
-- so that we can define the basic table definitions without needing to define
-- the transform ('zipBeamFieldsM') at the same time.
data family PrimaryKey (table :: (Type -> Type) -> Type) :: (Type -> Type) -> Type

class (Beamable table, Beamable (PrimaryKey table)) => Table table where

