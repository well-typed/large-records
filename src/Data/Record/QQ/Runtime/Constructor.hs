{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.QQ.Runtime.Constructor (
    RecordHasConstructor(..)
  ) where

import Data.Kind

-- | Typed uncurried constructor of the record
--
-- This is used internally by the quasi-quoter.
class RecordHasConstructor a where
  -- | Type of the constructor
  --
  -- We could compute this in terms of 'MetadataOf' but we don't do that in the
  -- interest of performance.
  --
  -- Given a record @R a b = MkR { field1 :: T1 a, field2 :: T2 b, .. }@, this is
  --
  -- > T1 a -> T2 b -> ... -> R a b
  type TypeOfConstr a :: Type

  -- | Get the record constructor
  --
  -- The first argument serves as a proxy. We ask for an actual value, which we
  -- will instantiate to @LR__MkR undefined@; we use this instead of a proxy,
  -- because this avoids having to deal with any type variables in the record
  -- type.
  --
  -- The strange name is meant to reflect that this is internal for internal
  -- use only (by the QQ).
  --
  -- This will take care of forcing the right fields.
  __recordConstructor :: a -> TypeOfConstr a

