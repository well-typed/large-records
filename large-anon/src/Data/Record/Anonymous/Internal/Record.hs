{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Full record representation
--
-- Intended for qualified import.
--
-- > import Data.Record.Anonymous.Internal.Record (Record)
-- > import qualified Data.Record.Anonymous.Internal.Record as Record
module Data.Record.Anonymous.Internal.Record (
    -- * Representation
    Record(..)
  , canonicalize
  , unsafeFromCanonical
    -- * Main API
  , Field(..)
  , empty
  , insert
  , get
  , set
  , merge
  , castRecord
  ) where

import Data.Kind
import Data.Proxy
import Data.Record.Generic.Rep.Internal (noInlineUnsafeCo)
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits

import Data.Record.Anonymous.Internal.Diff      (Diff)
import Data.Record.Anonymous.Internal.Canonical (Canonical)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Canonical as Canon
import qualified Data.Record.Anonymous.Internal.Diff      as Diff

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Anonymous record
--
-- A @Record f xs@ has a field @nm@ of type @f x@ for every @(nm, x)@ in @xs@.
--
-- To access fields of the record, either use the 'HasField' instances
-- (possibly using the record-dot-preprocessor to get record-dot syntax),
-- or using the simple wrappers 'get' and 'set'. The 'HasField' instances
-- are resolved by the plugin, so be sure to use
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
--
-- Let's consider a few examples. After we define
--
-- > example :: Record '[ '("a", Bool) ]
-- > example = insert #a True empty
--
-- we get
--
-- >>> get #a example -- or @example.a@ if using RecordDotSyntax
-- I True
--
-- >>> get #b example
-- ...
-- ...No instance for (HasField "b" (Record...
-- ...
--
-- >>> get #a example :: I Int
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
--
-- When part of the record is not known, it might not be possible to resolve a
-- 'HasField' constraint until later. For example, in
--
-- >>> (\r -> get #x r) :: Record I '[ '(f, a), '("x", b) ] -> I b
-- ...
-- ...No instance for (HasField "x" (...
-- ...
--
-- This is important, because if @f == "x"@, this would only be sound if also
-- @a == b@. We /could/ introduce a new constraint to say precisely that, but
-- it would have little benefit; instead we just leave the 'HasField' constraint
-- unresolved until we know more about the record.
data Record f (r :: [(Symbol, Type)]) = Record {
      recordDiff  :: Diff f
    , recordCanon :: Canonical f
    }

type role Record nominal representational

-- | Construct canonical form of the record (i.e., apply the internal 'Diff')
--
-- This is @O(n)@, and should be done only for operations on records that are
-- @O(n)@ /anyway/, so that the cost can be absorbed.
canonicalize :: Record f r -> Canonical f
canonicalize Record{..} = Diff.apply recordDiff recordCanon

-- | Construct 'Record' from 'Canonical' representation (empty 'Diff')
--
-- This function is unsafe because we cannot verify whether the record matches
-- it's row specification @r@.
unsafeFromCanonical :: Canonical f -> Record f r
unsafeFromCanonical canon = Record {
      recordDiff  = Diff.empty
    , recordCanon = canon
    }

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Proxy for a field name, with 'IsLabel' instance
--
-- The 'IsLabel' instance makes it possible to write
--
-- > #foo
--
-- to mean
--
-- > Field (Proxy @"foo")
data Field nm where
  Field :: KnownSymbol nm => Proxy nm -> Field nm

instance (nm ~ nm', KnownSymbol nm) => IsLabel nm' (Field nm) where
  fromLabel = Field (Proxy @nm)

-- | Empty record
empty :: Record f '[]
empty = Record Diff.empty mempty

-- | Insert new field
insert :: Field nm -> f a -> Record f r -> Record f ('(nm, a) ': r)
insert (Field nm) x r@Record{recordDiff} = r {
      recordDiff = Diff.insert (symbolVal nm) (co x) recordDiff
    }
  where
    co :: f a -> f Any
    co = noInlineUnsafeCo

-- | Get field from the record
--
-- This is just a wrapper around 'getField'
get :: forall nm f r a.
     HasField nm (Record f r) a
  => Field nm -> Record f r -> a
get _ = getField @nm @(Record f r)

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
set :: forall nm f r a.
     HasField nm (Record f r) a
  => Field nm -> a -> Record f r -> Record f r
set _ = flip (setField @nm @(Record f r))

-- | Merge two records
--
-- 'HasField' constraint can be resolved for merged records, subject to the same
-- condition discussed in the documentation of 'Record': since records are left
-- biased, all fields in the record must be known up to the requested field:
--
-- Simple example, completely known record:
--
-- >>> :{
--   let example :: Record I (Merge '[ '("a", Bool)] '[ '("b", Char)])
--       example = merge (insert #a (I True) empty) (insert #b (I 'a') empty)
--   in get #b example
-- :}
-- I 'a'
--
-- Slightly more sophisticated, only part of the record known:
--
-- >>> :{
--   let example :: Record I (Merge '[ '("a", Bool)] r) -> I Bool
--       example = get #a
--   in example (merge (insert #a (I True) empty) (insert #b (I 'a') empty))
-- :}
-- I True
--
-- Rejected example: first part of the record unknown:
--
-- >>> :{
--   let example :: Record I (Merge r '[ '("b", Char)]) -> I Char
--       example = get #b
--   in example (merge (insert #a (I True) empty) (insert #b (I 'a') empty))
-- :}
-- ...
-- ...No instance for (HasField "b" (...
-- ...
merge :: Record f r -> Record f r' -> Record f (Merge r r')
merge (canonicalize -> r) (canonicalize -> r') =
    unsafeFromCanonical $ r <> r'

    --Diff.apply (Diff.fromCanonical r) r'

-- | Cast record
--
-- Some examples of valid casts. We can cast a record to itself:
--
-- >>> castRecord example :: Record I '[ '("a", Bool) ]
-- Record {a = I True}
--
-- We can reorder fields:
--
-- >>> castRecord (insert #a (I True) $ insert #b (I 'a') $ empty) :: Record I '[ '("b", Char), '("a", Bool) ]
-- Record {b = I 'a', a = I True}
--
-- We can flatten merged records:
--
-- >>> castRecord (merge (insert #a (I True) empty) (insert #b (I 'a') empty)) :: Record I '[ '("a", Bool), '("b", Char) ]
-- Record {a = I True, b = I 'a'}
--
-- Some examples of invalid casts. We cannot change the types of the fields:
--
-- >>> castRecord example :: Record I '[ '("a", Int) ]
-- ...
-- ...Couldn't match...Bool...Int...
-- ...
--
-- We cannot drop fields:
--
-- >>> castRecord (insert #a (I True) $ insert #b (I 'a') $ empty) :: Record I '[ '("a", Bool) ]
-- ...
-- ...No instance for (Isomorphic...
-- ...
--
-- We cannot add fields:
--
-- >>> castRecord example :: Record I '[ '("a", Bool), '("b", Char) ]
-- ...
-- ...No instance for (Isomorphic...
-- ...
castRecord :: forall f r r'. Isomorphic r r' => Record f r -> Record f r'
castRecord (canonicalize -> r) =
    unsafeFromCanonical $
      Canon.project (indices (isomorphic (Proxy @r) (Proxy @r'))) r
  where
    -- TODO: If this works out, we should simply Permutation.
    -- (And maybe generalize 'castRecord' to be 'projectRecord')
    indices :: Permutation -> [Int]
    indices (Permutation p) = map snd p
