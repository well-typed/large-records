{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Canonical gecord (i.e., no diff)
--
-- Intended for qualified import.
--
-- > import Data.Record.Anonymous.Internal.Canonical (Canonical)
-- > import qualified Data.Record.Anonymous.Internal.Canonical as Canon
module Data.Record.Anonymous.Internal.Canonical (
    Canonical(..)
  , withShapeOf
    -- * Basic API
  , empty
  , insert
  , reshuffle
    -- * Indexed access
  , getAtIndex
  , setAtIndex
    -- * Conversion
  , toList
  , fromList
  , fromVector
    -- * Simple (non-constrained) combinators
    -- ** "Functor"
  , map
  , mapM
    -- ** Zipping
  , zipWith
  , zipWithA
  , zipWithM
    -- ** "Foldable"
  , collapse
    -- ** "Traversable"
  , sequenceA
    -- ** "Applicable"
  , ap
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)
import qualified Prelude

import Data.Bifunctor
import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.SOP.BasicFunctors
import Data.SOP.Classes (type (-.->)(apFn))
import Data.Vector (Vector, (//))
import GHC.Exts (Any)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector

import Data.Record.Anonymous.Internal.Row (Permutation(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Canonical gecord representation
--
-- Canonicity here refers to the fact that we have no @Diff@ to apply
-- (see "Data.Record.Anonymous.Internal.Diff").
--
-- NOTE: When we cite the algorithmic complexity of operations on 'Canonical',
-- we assume that 'HashMap' inserts and lookups are @O(1)@, which they are in
-- practice (especially given the relatively small size of typical records),
-- even if theoretically they are @O(log n)@. See also the documentation of
-- "Data.HashMap.Strict".
--
-- TODO: Deal with field strictness/laziness.
data Canonical f = Canonical {
      -- | All values in the record, in row order.
      --
      -- It is important that the vector is in row order: this is what makes
      -- it possible to define functions such as @mapM@ (for which ordering
      -- must be well-defined).
      canonValues :: !(Vector (f Any))

      -- | Field names in row order
      --
      -- Inserting a field into a record currently has type
      --
      -- > insert :: Field nm -> f a -> Record f r -> Record f ('(nm, a) ': r)
      --
      -- Specifically, it does /not/ have a 'Lacks' constraint (indeed, we do
      -- not have such a constraint in the library at present). This means that
      -- we do not know at compile time whether a particular call to 'insert' is
      -- a true insert or whether it shadows an existing field. We can recover
      -- this information from looking at 'canonFields'.
      --
      -- TODO: It might perhaps be possible to omit this field if we allowed
      -- shadowing also at the value level. If 'Canonical' is just a vector,
      -- translating back and forth to the generic representation might be
      -- easier (no need for 'KnownFields'). Not sure what the repercussions
      -- of value-level shadowing would be though.
    , canonFields :: [String]
    }

-- | Construct a new 'Canonical' with the same shape but new values.
--
-- Precondition: the vector must be of the right shape.
withShapeOf :: Canonical f -> Vector (g Any) -> Canonical g
withShapeOf c values = c { canonValues = values }

{-------------------------------------------------------------------------------
  Indexed access
-------------------------------------------------------------------------------}

-- | Get field at the specified index
--
-- @O(1)@.
getAtIndex :: Canonical f -> Int -> f Any
getAtIndex Canonical{canonValues} ix = canonValues Vector.! ix

-- | Set fields at the specified indices
--
-- @O(n)@ in the size of the record (independent of the number of field updates)
-- @O(1)@ if the list of updates is empty.
setAtIndex :: [(Int, f Any)] -> Canonical f -> Canonical f
setAtIndex [] c = c
setAtIndex fs c@Canonical{canonValues} = c { canonValues = canonValues // fs }

{-------------------------------------------------------------------------------
  Basic API
-------------------------------------------------------------------------------}

-- | Empty record
empty :: Canonical f
empty = Canonical {
      canonValues = Vector.empty
    , canonFields = []
    }

-- | Insert fields into the record
--
-- The list may contain duplicate fields, in which case fields earlier in the
-- list will shadow values later fields. Similarly, fields in the list will
-- shadow any duplicate fields already present in the record.
--
-- @O(n)@ in the number of inserts and the size of the record.
-- @O(1)@ if the list of inserts is empty.
insert :: [(String, f Any)] -> Canonical f -> Canonical f
insert []  = id
insert new = fromList . (new ++) . toList

-- | Reshuffle the fields in the record
--
-- Precondition: the new fields must be a permutation of the old.
--
-- @(n)@.
--
-- Implementation note: reshuffling /must/ create a new array, because the
-- array must be in row order (so that we can traverse it in order).
reshuffle :: forall f. Permutation -> Canonical f -> Canonical f
reshuffle (Permutation perm) c =
    fromList $ Prelude.map aux perm
  where
    aux :: (String, Int) -> (String, f Any)
    aux (nm, i) = (nm, getAtIndex c i)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | All fields in row order
--
-- @O(n)@
toList :: Canonical f -> [(String, f Any)]
toList c@Canonical{canonFields} =
    Prelude.map (second (getAtIndex c)) (Prelude.zip canonFields [0..])

-- | Construct record from list of named fields in row order
--
-- The list may contain duplicates, in which case fields later in the list are
-- shadowed by fields earlier in the list.
--
-- @O(n)@.
fromList :: [(String, f Any)] -> Canonical f
fromList = go [] HashMap.empty [] 0
    where
      go :: [f Any]             -- Accumulated values, in reverse row order
         -> HashMap String Int  -- Accumulated indices
         -> [String]            -- Accumulated fields, in reverse row order
         -> Int                 -- Next available index
         -> [(String, f Any)]
         -> Canonical f
      go accValues _accIndices accFields !nextIx [] = Canonical {
            canonValues = Vector.fromListN nextIx (reverse accValues)
          , canonFields = reverse accFields
          }
      go accValues accIndices accFields !nextIx ((f, x):fs) =
          case HashMap.lookup f accIndices of
            Just _  -> go accValues accIndices accFields nextIx fs -- shadowed
            Nothing -> go (x : accValues)
                          (HashMap.insert f nextIx accIndices)
                          (f : accFields)
                          (succ nextIx)
                          fs

-- | From already constructed vector
--
-- Precondition: both the list of names and the vector must have the right
-- shape. Unlike 'fromList', this does /not/ take care of shadowing.
fromVector :: [String] -> Vector (f Any) -> Canonical f
fromVector names values = Canonical {
      canonValues = values
    , canonFields = names
    }

{-------------------------------------------------------------------------------
  Simple (non-constrained) combinators
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Canonical f -> Canonical g
map f c = withShapeOf c $ fmap f (canonValues c)

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Canonical f -> m (Canonical g)
mapM f c = fmap (withShapeOf c) $ traverse f (canonValues c)

-- | Zip two records
--
-- Precondition: the two records must have the same shape.
zipWith ::
     (forall x. f x -> g x -> h x)
  -> Canonical f -> Canonical g -> Canonical h
zipWith f c c' = withShapeOf c $
    Vector.zipWith f (canonValues c) (canonValues c')

-- | Applicative zip of two records
--
-- See also 'zipWithM', which avoids one vector copy.
zipWithA ::
     Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> Canonical f -> Canonical g -> m (Canonical h)
zipWithA f c c' = sequenceA $ zipWith (\x y -> Comp $ f x y) c c'

-- | Monadic zip of two records
--
-- Precondition: the two records must have the same shape.
zipWithM ::
     Monad m
  => (forall x. f x -> g x -> m (h x))
  -> Canonical f -> Canonical g -> m (Canonical h)
zipWithM f c c' = fmap (withShapeOf c) $
    Vector.zipWithM f (canonValues c) (canonValues c')

collapse :: Canonical (K a) -> [a]
collapse c = co $ Vector.toList (canonValues c)
  where
    co :: [K a Any] -> [a]
    co = coerce

sequenceA :: Applicative m => Canonical (m :.: f) -> m (Canonical f)
sequenceA c = fmap (withShapeOf c) $ traverse unComp $ canonValues c

ap :: Canonical (f -.-> g) -> Canonical f -> Canonical g
ap = zipWith apFn
