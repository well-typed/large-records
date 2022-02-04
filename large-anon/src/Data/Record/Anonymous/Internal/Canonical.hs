{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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
  , get
  , set
  , insert
  , reshuffle
    -- * Indexed access
  , indexOf
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

      -- | Map from field names to indices of the vector
      --
      -- Invariants:
      --
      -- * Every field is mapped to a different index
      --
      --   >     Map.lookup f canonIndices == Map.lookup f' canonIndices
      --   > ==> f == f'
      --
      -- * The codomain of 'canonIndices' is isomorphic to the domain of
      --   'canonValues'
      --
      --   >    sort (Map.elems canonIndices)
      --   > == [0 .. Vector.length canonValues - 1]
    , canonIndices :: !(HashMap String Int)

      -- | Field names and indices, in row order
      --
      -- We can compute this from 'canonIndices', but at @O(n log n)@ cost:
      --
      -- > canonFields == sortOn snd (Map.toList canonIndices)
      --
      -- TODO: Do we really need that Int?
    , canonFields :: [(String, Int)]
    }

-- | Construct a new 'Canonical' with the same shape but new values.
--
-- Precondition: the vector must be of the right shape.
withShapeOf :: Canonical f -> Vector (g Any) -> Canonical g
withShapeOf c values = c { canonValues = values }

{-------------------------------------------------------------------------------
  Indexed access
-------------------------------------------------------------------------------}

-- | Index of the given field
--
-- Precondition: the field must exist.
--
-- @O(1)@.
indexOf :: Canonical f -> String -> Int
indexOf Canonical{canonIndices} f = canonIndices HashMap.! f

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
      canonValues  = Vector.empty
    , canonIndices = HashMap.empty
    , canonFields  = []
    }

-- | Get field from the record
--
-- Precondition: the field must be present.
--
-- @O(1)@.
get :: String -> Canonical f -> f Any
get f c = getAtIndex c (indexOf c f)

-- | Update fields from the record
--
-- Precondition: the fields must be present.
--
-- @O(n)@ in the size of the record, independent of the number of updates.
set :: [(String, f Any)] -> Canonical f -> Canonical f
set fs c = setAtIndex (Prelude.map (first (indexOf c)) fs) c

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
reshuffle :: [String] -> Canonical f -> Canonical f
reshuffle newOrder c =
    fromList $ Prelude.map (\nm -> (nm, get nm c)) newOrder

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | All fields in row order
--
-- @O(n)@
toList :: Canonical f -> [(String, f Any)]
toList c@Canonical{canonFields} =
    Prelude.map (second (getAtIndex c)) canonFields

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
         -> [(String, Int)]     -- Accumulated fields, in reverse row order
         -> Int                 -- Next available index
         -> [(String, f Any)]
         -> Canonical f
      go accValues accIndices accFields !nextIx [] = Canonical {
            canonValues  = Vector.fromListN nextIx (reverse accValues)
          , canonIndices = accIndices
          , canonFields  = reverse accFields
          }
      go accValues accIndices accFields !nextIx ((f, x):fs) =
          case HashMap.lookup f accIndices of
            Just _  -> go accValues accIndices accFields nextIx fs -- shadowed
            Nothing -> go (x : accValues)
                          (HashMap.insert f nextIx accIndices)
                          ((f, nextIx) : accFields)
                          (succ nextIx)
                          fs

-- | From already constructed vector
--
-- Precondition: both the list of names and the vector must have the right
-- shape. Unlike 'fromList', this does /not/ take care of shadowing.
fromVector :: [String] -> Vector (f Any) -> Canonical f
fromVector names values = Canonical {
      canonValues  = values
    , canonIndices = HashMap.fromList fields
    , canonFields  = fields
    }
  where
    fields :: [(String, Int)]
    fields = Prelude.zip names [0..]

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
