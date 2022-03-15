{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , fromCanonical
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import GHC.Exts (Any)

import qualified Data.HashMap.Strict as HashMap

import Data.Record.Anonymous.Internal.Canonical (Canonical(..))

import qualified Data.Record.Anonymous.Internal.Canonical as Canon

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
-- NOTE: As for 'Canonical', when citing algorithmic complexity of operations on
-- 'Diff', we assume that 'HashMap' inserts and lookups are @O(1)@. See
-- 'Canonical' for more detailed justification.
data Diff f = Diff {
      -- | New values of existing fields
      --
      -- Indices refer to the original record.
      diffUpd :: HashMap Int (f Any)

      -- | Values of newly inserted fields
    , diffNew :: HashMap String (f Any)

      -- | List of new fields, most recently inserted first
      --
      -- May contain duplicates: fields inserted later shadow earlier fields.
      --
      -- We do not remove duplicates during 'Diff' construction, as this would
      -- result in @O(n²)@ complexity of insert. Instead we deal with this in
      -- a single sweep in 'apply'.
    , diffIns :: [String]
    }

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
    , diffNew = HashMap.empty
    , diffIns = []
    }

-- | Get field
--
-- Precondition: field must be present in the diff or in the record.
-- Postcondition:
--
-- > Diff.get f d c == Canon.get f (Diff.apply d c)
--
-- @O(1)@.
get :: (Int, String) -> Diff f -> Canonical f -> f Any
get (i, f) Diff{..} c =
    case HashMap.lookup f diffNew of
      Just x  -> x                                   -- inserted  in the diff
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
set :: (Int, String) -> f Any -> Diff f -> Diff f
set (i, f) x d@Diff{..} =
    case tryUpdateHashMap f (const x) diffNew of
      Just diffNew' -> d { diffNew = diffNew' }
      Nothing       -> d { diffUpd = HashMap.insert i x diffUpd }

-- | Insert new field
--
-- Precondition: none (if the field already exists, it will be shadowed).
-- Postcondition:
--
-- > Diff.apply (Diff.insert f x d) c = Canon.insert [(f, x)] (apply d c)
--
-- @(1)@.
insert :: String -> f Any -> Diff f -> Diff f
insert f x d@Diff{..} = d {
      diffNew = HashMap.insert f x diffNew
    , diffIns = f : diffIns
    }

{-------------------------------------------------------------------------------
  Batch operations
-------------------------------------------------------------------------------}

-- | Apply diff
--
-- @O(n)@ in general, but @O(1)@ if the `Diff` is empty.
apply :: forall f. Diff f -> Canonical f -> Canonical f
apply d =
      Canon.insert     (diffIns' d)
    . Canon.setAtIndex (diffUpd' d)
  where
    diffUpd' :: Diff f -> [(Int, f Any)]
    diffUpd' = HashMap.toList . diffUpd

    -- We only store a /single/ value for each inserted field. If therefore
    -- there are fields being shadowed (i.e., the same value inserted more than
    -- once), each of those fields will be associated with the /same/ value
    -- here. This doesn't matter: only the first occurrence of each field is
    -- used by 'Canon.insert'.
    diffIns' :: Diff f -> [(String, f Any)]
    diffIns' = map (\f -> (f, diffNew d HashMap.! f)) . diffIns

-- | Construct a 'Diff' from a 'Canonical' record
--
-- Primary use case for this is merging two records together: the first record
-- becomes a 'Diff' to the second.
--
-- Post condition:
--
-- > apply (fromCanonical c) empty == c
--
-- @O(n)@
fromCanonical :: Canonical f -> Diff f
fromCanonical c = Diff {
      diffUpd = HashMap.empty
    , diffNew = HashMap.fromList (Canon.toList c)
    , diffIns = canonFields c
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Attempt to update the map at the specified key
--
-- If the key does not exist, returns 'Nothing'
--
-- @O(1)@.
tryUpdateHashMap :: forall k a.
     (Hashable k, Eq k)
  => k -> (a -> a) -> HashMap k a -> Maybe (HashMap k a)
tryUpdateHashMap k f = HashMap.alterF f' k
  where
    -- The /outer/ 'Maybe' here is the functor @f@ argument to 'alterF', so if
    -- we return 'Nothing', the whole thing fails. We want to do this when the
    -- key is not present. We never want to return 'Nothing' for the /inner/
    -- 'Maybe', because if the key /is/ present, it should not be deleted.
    f' :: Maybe a -> Maybe (Maybe a)
    f' Nothing  = Nothing
    f' (Just x) = Just (Just (f x))

