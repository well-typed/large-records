{-# LANGUAGE RankNTypes #-}
-- | Utility functions for working with beam
--
-- These are not exported from Data.Record.Beam, and should be considered
-- internal use only.
module Data.Record.Beam.Internal (
    -- * Working with Columnar'
    liftColumnarA2
  , liftNullableA2
  ) where

import Data.Coerce (coerce)
import Data.Proxy
import Database.Beam.Schema.Tables

{-------------------------------------------------------------------------------
  Working with Columnar'
-------------------------------------------------------------------------------}

liftColumnarA2 ::
     Functor m
  => Proxy x
  -> (Columnar' f x -> Columnar' g x -> m (Columnar' h x))
  -> (Columnar  f x -> Columnar  g x -> m (Columnar  h x))
liftColumnarA2 _ f fx gx = getColumnar' <$> f (Columnar' fx) (Columnar' gx)

liftNullableA2 ::
     Functor m
  => (forall x. Columnar'           f  x -> Columnar'           g  x -> m (Columnar'           h  x))
  -> (forall x. Columnar' (Nullable f) x -> Columnar' (Nullable g) x -> m (Columnar' (Nullable h) x))
liftNullableA2 f x y = toNullable <$> f (fromNullable x) (fromNullable y)
  where
    toNullable :: Columnar' w (Maybe a) -> Columnar' (Nullable w) a
    toNullable = coerce

    fromNullable :: Columnar' (Nullable w) a -> Columnar' w (Maybe a)
    fromNullable = coerce

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

getColumnar' :: Columnar' f a -> Columnar f a
getColumnar' (Columnar' x) = x

