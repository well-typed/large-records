{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    Record -- opaque
  , toCanonical
  , unsafeFromCanonical
    -- * Main API
  , Field(..)
  , empty
  , insert
  , insertA
  , get
  , set
  , merge
  , lens
  , project
  , applyDiff
    -- * Support for @typelet@
  , letRecordT
  , letInsertAs
  ) where

import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Kind
import Data.Proxy
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits
import TypeLet.UserAPI

import Data.Record.Anon.Core.Canonical (Canonical)
import Data.Record.Anon.Core.Diff (Diff)
import Data.Record.Anon.Core.FieldName (FieldName(..))
import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Core.Canonical as Canon
import qualified Data.Record.Anon.Core.Diff      as Diff
import qualified Data.Record.Anon.Core.Record    as Core (Record(..))
import qualified Data.Record.Anon.Core.Record    as Core.Record

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Record (f :: k -> Type) (r :: Row k) = Wrap { unwrap :: Core.Record f }

pattern Record :: Diff f -> Int -> Canonical f -> Record f r
pattern Record {recordDiff, recordDiffSize, _recordCanon} =
    Wrap (Core.Record recordDiff recordDiffSize _recordCanon)

{-# COMPLETE Record #-}

toCanonical :: Record f r -> Canonical f
toCanonical = Core.Record.toCanonical . unwrap

-- | Construct 'Record' from 'Canonical' representation (empty 'Diff')
--
-- This function is unsafe because we cannot verify whether the record matches
-- it's row specification @r@.
unsafeFromCanonical :: Canonical f -> Record f r
unsafeFromCanonical = Wrap . Core.Record.fromCanonical

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance forall k (n :: Symbol) (f :: k -> Type) (r :: Row k) (a :: Type).
       RecordHasField n f r a
    => HasField n (Record f r) a where
  hasField = aux $ recordHasField (Proxy @n) (Proxy @r)
    where
      aux ::
           (Core.Record f   -> (a -> Core.Record f  , a))
        -> (     Record f r -> (a ->      Record f r, a))
      aux = coerce

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
data Field n where
  Field :: (KnownSymbol n, KnownHash n) => Proxy n -> Field n

instance (n ~ n', KnownSymbol n, KnownHash n) => IsLabel n' (Field n) where
  fromLabel = Field (Proxy @n)

-- | Empty record
empty :: Record f '[]
empty = Record Diff.empty 0 mempty

-- | Insert new field
insert :: Field n -> f a -> Record f r -> Record f (n := a : r)
insert (Field n) x r@Record{ recordDiff     = diff
                           , recordDiffSize = diffSize
                           } = r {
      recordDiff     = Diff.insert (mkFieldName n) (co x) diff
    , recordDiffSize = diffSize + 1
    }
  where
    co :: f a -> f Any
    co = noInlineUnsafeCo

    -- | Compile-time construction of a 'FieldName'
    mkFieldName :: (KnownSymbol s, KnownHash s) => Proxy s -> FieldName
    mkFieldName p = FieldName (hashVal p) (symbolVal p)

-- | Applicative insert
--
-- This is a simple wrapper around 'insert', but can be quite useful when
-- constructing records. Consider code like
--
-- > foo :: m (a, b, c)
-- > foo = (,,) <$> action1
-- >            <*> action2
-- >            <*> action3
--
-- We cannot really extend this to the world of named records, but we /can/
-- do something comparable using anonymous records:
--
-- > foo :: m (Record f '[ "x" := a, "y" := b, "z" := c ])
-- >    insertA #x action1
-- >  $ insertA #y action2
-- >  $ insertA #z action3
-- >  $ pure Anon.empty
insertA ::
     Applicative m
  => Field n -> m (f a) -> m (Record f r) -> m (Record f (n := a : r))
insertA f x r = insert f <$> x <*> r

-- | Get field from the record
--
-- This is just a wrapper around 'getField'
get :: forall n f r a.
     HasField n (Record f r) a
  => Field n -> Record f r -> a
get _ = getField @n @(Record f r)

-- | Update field in the record
--
-- This is just a wrapper around 'setField'.
set :: forall n f r a.
     HasField n (Record f r) a
  => Field n -> a -> Record f r -> Record f r
set _ = flip (setField @n @(Record f r))

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
merge (toCanonical -> r) (toCanonical -> r') =
    unsafeFromCanonical $ r <> r'

-- | Lens from one record to another
--
-- TODO: Update docs (these are still from the old castRecord).
-- TODO: Make all doctests work agian.
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
lens :: forall f r r'.
     Project f r r'
  => Record f r -> (Record f r', Record f r' -> Record f r)
lens = \(toCanonical -> r) ->
    bimap getter setter $
      Canon.lens (projectIndices (Proxy @f) (Proxy @r) (Proxy @r')) r
  where
    getter :: Canonical f -> Record f r'
    getter = unsafeFromCanonical

    setter :: (Canonical f -> Canonical f) -> Record f r' -> Record f r
    setter f (toCanonical -> r) = unsafeFromCanonical (f r)

-- | Project out subrecord
--
-- This is just @fst . lens@.
project :: Project f r r' => Record f r -> Record f r'
project = fst . lens

-- | Apply all pending changes to the record
--
-- Updates on a record are stored in a hash table. As this hashtable grows,
-- record field access and update will become more expensive. Applying the
-- updates, resulting in a flat vector, is an @O(n)@ operation. This will happen
-- automatically whenever another @O(n)@ operation is applied (for example,
-- mapping a function over the record). However, cccassionally it is useful to
-- explicitly apply these changes, for example after constructing a record or
-- updating a lot of fields.
applyDiff :: Record f r -> Record f r
applyDiff (toCanonical -> r) = unsafeFromCanonical r

{-------------------------------------------------------------------------------
  Support for @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
--
-- This can be used in conjunction with 'letInsertAs':
--
-- > example :: Record I '[ "a" := Int, "b" := Char, "c" := Bool ]
-- > example = letRecordT $ \p -> castEqual $
-- >     letInsertAs p #c (I True) empty $ \xs02 ->
-- >     letInsertAs p #b (I 'X' ) xs02  $ \xs01 ->
-- >     letInsertAs p #a (I 1   ) xs01  $ \xs00 ->
-- >     castEqual xs00
letRecordT :: forall r f.
     (forall r'. Let r' r => Proxy r' -> Record f r)
  -> Record f r
letRecordT f = letT' (Proxy @r) f

-- | Insert field into a record and introduce type variable for the result
letInsertAs :: forall r r' f n a.
     Proxy r       -- ^ Type of the record we are constructing
  -> Field n       -- ^ New field to be inserted
  -> f a           -- ^ Value of the new field
  -> Record f r'   -- ^ Record constructed so far
  -> (forall r''. Let r'' (n := a : r') => Record f r'' -> Record f r)
                   -- ^ Assign type variable to new partial record, and continue
  -> Record f r
letInsertAs _ n x r = letAs' (insert n x r)



