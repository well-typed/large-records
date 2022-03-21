{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- {-# OPTIONS_GHC -fprint-explicit-kinds #-}

-- | Record diff
--
-- Intended for qualified import.
--
-- > import Data.Record.Anonymous.Internal.Diff (Diff)
-- > import qualified Data.Record.Anonymous.Internal.Diff as Diff
module Data.Record.Anonymous.Internal.Diff (
    Diff(..)
    -- * Incremental construction
  , empty
  , get
  , set
  , insert
    -- * Batch operations
  , apply
    -- * Debugging support
  , toString
  ) where

-- import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.SOP.BasicFunctors
import Debug.RecoverRTTI (AnythingToString(..))
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty  as NE
import qualified Data.HashMap.Strict as HashMap

import Data.Record.Anonymous.Internal.Canonical (Canonical(..))
import Data.Record.Anonymous.Internal.FieldName (FieldName)

import qualified Data.Record.Anonymous.Internal.Canonical    as Canon
import qualified Data.Record.Anonymous.Internal.Util.HashMap as HashMap

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Record changes to a ('Canonical') record.
--
-- Unlike 'Canon.set' and 'Canon.insert', 'Diff.set' and 'Diff.insert' deal with
-- a single field at a time, at @O(1)@ cost. This is the raison d'être of
-- 'Diff': amortize the cost of repeated updates/inserts. Specifically, a series
-- of inserts or updates will build a 'Diff' which will take @O(n)@ to apply,
-- but that 'apply' should be /executed/ only when we do an operation which is
-- @O(n)@ anyway, thereby absorbing the cost.
--
-- This is also the reason that 'Diff' is name based, not index based: inserting
-- a new field would increase all indices of existing fields by 1, an inherently
-- @O(n)@ operation.
--
-- NOTE: As for 'Canonical', when citing algorithmic complexity of operations on
-- 'Diff', we assume that 'HashMap' inserts and lookups are @O(1)@. See
-- 'Canonical' for more detailed justification.
--
-- NOTE: Since @large-anon@ currently only supports records with strict fields,
-- we use strict 'HashMap' here.
data Diff (f :: k -> Type) = Diff {
      -- | New values of existing fields
      --
      -- Indices refer to the original record.
      diffUpd :: HashMap Int (f Any)

      -- | List of new fields, most recently inserted first
      --
      -- May contain duplicates: fields inserted later shadow earlier fields.
    , diffIns :: [FieldName]

      -- | Values for the newly inserted fields
      --
      -- If the field is shadowed, the list will have multiple entries. Entries
      -- in the lists are from new to old, so the head of the list is the
      -- "currently visible" entry.
    , diffNew :: HashMap FieldName (NonEmpty (f Any))
    }

type role Diff representational

deriving instance Show a => Show (Diff (K a))

{-------------------------------------------------------------------------------
  Incremental construction

  TODO: We should property check these postconditions.
-------------------------------------------------------------------------------}

-- | Empty difference
--
-- Postcondition:
--
-- > apply empty c == c
empty :: Diff f
empty = Diff {
      diffUpd = HashMap.empty
    , diffIns = []
    , diffNew = HashMap.empty
    }

-- | Get field
--
-- Precondition: field must be present in the diff or in the record.
-- Postcondition:
--
-- > Diff.get f d c == Canon.get f (Diff.apply d c)
--
-- @O(1)@.
get :: (Int, FieldName) -> Diff f -> Canonical f -> f Any
get (i, f) Diff{..} c =
    case HashMap.lookup f diffNew of
      Just xs -> NE.head xs                          -- inserted  in the diff
      Nothing -> case HashMap.lookup i diffUpd of
                   Just x  -> x                      -- updated   in the diff
                   Nothing -> Canon.getAtIndex c i   -- unchanged in the diff

-- | Update existing field
--
-- Precondition: field must be present in the diff or in the record.
-- Postcondition:
--
-- > Diff.apply (Diff.set f x c d) c == Canon.set [(f, x)] (apply d c)
--
-- It is useful to spell out what happens when inserts and updated are mixed:
--
-- * When a field is inserted and then updated, we just update the corresponding
--   entry in 'diffNew'.
-- * When an /existing/ field is first updated and then a new field with the
--   same name is added, an entry is added to 'diffNew' but 'diffUpd' will also
--   contain an entry for this field. This doesn't matter: when the diff is
--   applied, the new field will shadow the old, and when we 'get' the value
--   of a field, we similarly /first/ check 'diffNew'.
-- * When the /same/ field is inserted more than once, updates to that field
--   will effectively affect all of them (since we store only a single value),
--   but only the first value will matter as it will shadow all the others.
--
-- @O(1)@.
set :: forall f. (Int, FieldName) -> f Any -> Diff f -> Diff f
set (i, f) x d@Diff{..} =
    case HashMap.alterExisting f updateInserted diffNew of
      Just ((), diffNew') -> d { diffNew = diffNew' }
      Nothing             -> d { diffUpd = HashMap.insert i x diffUpd }
  where
    updateInserted :: NonEmpty (f Any) -> ((), Maybe (NonEmpty (f Any)))
    updateInserted (_ :| prev) = ((), Just (x :| prev))

-- | Insert new field
--
-- Precondition: none (if the field already exists, it will be shadowed).
-- Postcondition:
--
-- > Diff.apply (Diff.insert f x d) c = Canon.insert [(f, x)] (apply d c)
--
-- @(1)@.
insert :: forall f. FieldName -> f Any -> Diff f -> Diff f
insert f x d@Diff{..} = d {
      diffIns = f : diffIns
    , diffNew = HashMap.alter (Just . insertField) f diffNew
    }
  where
    insertField :: Maybe (NonEmpty (f Any)) -> NonEmpty (f Any)
    insertField Nothing     = x :| []
    insertField (Just prev) = x <| prev

{-------------------------------------------------------------------------------
  Batch operations
-------------------------------------------------------------------------------}

-- | All new fields (including shadowed fields), from new to old
--
-- @O(n)@.
allNewFields :: Diff f -> [f Any]
allNewFields = \Diff{..} -> go diffNew diffIns
  where
    go :: HashMap FieldName (NonEmpty (f Any)) -> [FieldName] -> [f Any]
    go _  []     = []
    go vs (x:xs) = case HashMap.alterExisting x NE.uncons vs of
                     Nothing       -> error "allNewFields: invariant violation"
                     Just (v, vs') -> v : go vs' xs

-- | Apply diff
--
-- @O(n)@ in the size of the 'Canonical' and the 'Diff' in general.
-- @O(1)@ if the `Diff` is empty.
apply :: forall f. Diff f -> Canonical f -> Canonical f
apply d =
      Canon.insert     (allNewFields d)
    . Canon.setAtIndex (HashMap.toList (diffUpd d))

{-------------------------------------------------------------------------------
  Debugging support
-------------------------------------------------------------------------------}



toString :: forall k (f :: k -> Type). Diff f -> String
toString = show . mapDiff (K . AnythingToString . co)
  where
    mapDiff :: (forall x. f x -> g x) -> Diff f -> Diff g
    mapDiff f Diff{..} = Diff{
          diffUpd = fmap f diffUpd
        , diffIns = diffIns
        , diffNew = fmap (fmap f) diffNew
        }

    co :: f x -> f Any
    co = unsafeCoerce

{-
    -- This definition should work, but doesn't. Not sure why:
    aux :: forall. Diff f -> Diff ((K (AnythingToString (f Any))) :: k -> Type)
    aux = coerce
-}