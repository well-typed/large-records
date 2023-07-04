{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RoleAnnotations            #-}

-- | Canonical gecord (i.e., no diff)
--
-- Intended for qualified import.
--
-- > import Data.Record.Anonymous.Internal.Canonical (Canonical)
-- > import qualified Data.Record.Anonymous.Internal.Canonical as Canon
module Data.Record.Anon.Internal.Core.Canonical (
    Canonical -- opaque
    -- * Indexed access
  , getAtIndex
  , setAtIndex
    -- * Conversion
  , toList
  , fromList
  , toVector
  , fromVector
    -- * Basic API
  , insert
  , lens
    -- * Simple (non-constrained) combinators
  , map
  , mapM
  , zipWith
  , zipWithM
  , collapse
  , sequenceA
  , ap
    -- * Debugging support
#if DEBUG
  , toString
#endif
  ) where

import Prelude hiding (map, mapM, zipWith, sequenceA, pure)

import Data.Coerce (coerce)
import Data.Kind
import Data.SOP.BasicFunctors
import Data.SOP.Classes (type (-.->)(apFn))
import GHC.Exts (Any)

#if DEBUG
import Debug.RecoverRTTI (AnythingToString(..))
#endif

import qualified Data.Foldable as Foldable

import Data.Record.Anon.Internal.Util.StrictArray (StrictArray)

import qualified Data.Record.Anon.Internal.Util.StrictArray as Strict

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Canonical record representation
--
-- Canonicity here refers to the fact that we have no @Diff@ to apply (see
-- "Data.Record.Anonymous.Internal.Diff"). In this case, the record is
-- represented as a strict vector in row order (@large-anon@ is strict by
-- default; lazy records can be achieved using boxing). This order is important:
-- it makes it possible to define functions such as @mapM@ (for which ordering
-- must be well-defined).
--
-- Type level shadowing is reflected at the term level: if a record has
-- duplicate fields in its type, it will have multiple entries for that field
-- in the vector.
--
-- TODO: Currently we have no way of recovering the value of shadowed fields,
-- adding an API for that is future work. The work by Daan Leijen on scoped
-- labels might offer some inspiration there.
--
-- NOTE: When we cite the algorithmic complexity of operations on 'Canonical',
-- we assume that 'HashMap' inserts and lookups are @O(1)@, which they are in
-- practice (especially given the relatively small size of typical records),
-- even if theoretically they are @O(log n)@. See also the documentation of
-- "Data.HashMap.Strict".
newtype Canonical (f :: k -> Type) = Canonical {
      -- | To strict vector
      toVector :: StrictArray Strict.ZeroBasedIndex (f Any)
    }
  deriving newtype (Semigroup, Monoid)

type role Canonical representational

deriving instance Show a => Show (Canonical (K a))

{-------------------------------------------------------------------------------
  Indexed access
-------------------------------------------------------------------------------}

-- | Get field at the specified index
--
-- @O(1)@.
getAtIndex :: Canonical f -> Int -> f Any
getAtIndex (Canonical c) ix = c Strict.! Strict.ZeroBasedIndex ix

-- | Set fields at the specified indices
--
-- @O(n)@ in the size of the record (independent of the number of field updates)
-- @O(1)@ if the list of updates is empty.
setAtIndex :: [(Int, f Any)] -> Canonical f -> Canonical f
setAtIndex [] c             = c
setAtIndex fs (Canonical v) = Canonical (v Strict.// co fs)
  where
    co :: [(Int, f Any)] -> [(Strict.ZeroBasedIndex, f Any)]
    co = coerce

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | From strict vector
fromVector :: StrictArray Strict.ZeroBasedIndex (f Any) -> Canonical f
fromVector = Canonical

-- | All fields in row order
--
-- @O(n)@
toList :: Canonical f -> [f Any]
toList = Foldable.toList . toVector

-- | From list of fields in row order
--
-- @O(n)@.
fromList :: [f Any] -> Canonical f
fromList = fromVector . Strict.fromList

{-------------------------------------------------------------------------------
  Basic API
-------------------------------------------------------------------------------}

-- | Insert fields into the record
--
-- It is the responsibility of the caller to make sure that the linear
-- concatenation of the new fields to the existing record matches the row order
-- of the new record.
--
-- @O(n)@ in the number of inserts and the size of the record.
-- @O(1)@ if the list of inserts is empty.
insert :: forall f. [f Any] -> Canonical f -> Canonical f
insert []  = id
insert new = prepend
  where
     prepend :: Canonical f -> Canonical f
     prepend (Canonical v) = Canonical (Strict.fromList new <> v)

-- | Project out some fields in the selected order
--
-- It is the responsibility of the caller that the list of indices is in row
-- order of the new record.
--
-- @O(n)@ (in both directions)
lens :: [Int] -> Canonical f -> (Canonical f, Canonical f -> Canonical f)
lens is (Canonical v) = (
      Canonical $
        Strict.backpermute v (co is)
    , \(Canonical v') -> Canonical $
         Strict.update v (zip (co is) $ Foldable.toList v')
    )
  where
    co :: [Int] -> [Strict.ZeroBasedIndex]
    co = coerce

{-------------------------------------------------------------------------------
  Simple (non-constrained) combinators
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Canonical f -> Canonical g
map f (Canonical v) = Canonical $ fmap f v

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Canonical f -> m (Canonical g)
mapM f (Canonical v) = Canonical <$> Strict.mapM f v

-- | Zip two records
--
-- Precondition: the two records must have the same shape.
zipWith ::
     (forall x. f x -> g x -> h x)
  -> Canonical f -> Canonical g -> Canonical h
zipWith f (Canonical v) (Canonical v') = Canonical $ Strict.zipWith f v v'

-- | Applicative zip of two records
--
-- Precondition: the two records must have the same shape.
zipWithM ::
     Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> Canonical f -> Canonical g -> m (Canonical h)
zipWithM f (Canonical v) (Canonical v') = Canonical <$> Strict.zipWithM f v v'

collapse :: Canonical (K a) -> [a]
collapse (Canonical v) = co $ Foldable.toList v
  where
    co :: [K a Any] -> [a]
    co = coerce

sequenceA :: Applicative m => Canonical (m :.: f) -> m (Canonical f)
sequenceA (Canonical v) = Canonical <$> Strict.mapM unComp v

ap :: Canonical (f -.-> g) -> Canonical f -> Canonical g
ap = zipWith apFn

{-------------------------------------------------------------------------------
  Debugging support
-------------------------------------------------------------------------------}

#if DEBUG
toString :: forall k (f :: k -> Type). Canonical f -> String
toString = show . aux
  where
    aux :: Canonical f -> Canonical (K (AnythingToString (f Any)))
    aux = coerce
#endif