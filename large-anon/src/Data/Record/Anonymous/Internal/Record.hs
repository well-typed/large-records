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
  , applyPending
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

-- | Anonymous record
--
-- A @Record f r@ has a field @n@ of type @f x@ for every @(n := x)@ in @r@.
--
-- To construct a 'Record', use 'Data.Record.Anon.Advanced.insert' and
-- 'Data.Record.Anon.Advanced.empty', or use the @ANON_F@ syntax. See
-- 'Data.Record.Anon.Advanced.insert' for examples.
--
-- To access fields of the record, either use the 'GHC.Records.Compat.HasField'
-- instances (possibly using the @record-dot-preprocessor@), or using
-- 'Data.Record.Anon.Advanced.get' and 'Data.Record.Anon.Advanced.set'.
--
-- Remember to enable the plugin when working with anonymous records:
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
--
-- NOTE: If you do not need the functor parameter, see
-- "Data.Record.Anon.Simple" for a simplified interface.
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

empty :: Record f '[]
empty = Record Diff.empty 0 mempty

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

insertA ::
     Applicative m
  => Field n -> m (f a) -> m (Record f r) -> m (Record f (n := a : r))
insertA f x r = insert f <$> x <*> r

get :: forall n f r a.
     HasField n (Record f r) a
  => Field n -> Record f r -> a
get _ = getField @n @(Record f r)

set :: forall n f r a.
     HasField n (Record f r) a
  => Field n -> a -> Record f r -> Record f r
set _ = flip (setField @n @(Record f r))

merge :: Record f r -> Record f r' -> Record f (Merge r r')
merge (toCanonical -> r) (toCanonical -> r') =
    unsafeFromCanonical $ r <> r'

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

applyPending :: Record f r -> Record f r
applyPending (toCanonical -> r) = unsafeFromCanonical r

{-------------------------------------------------------------------------------
  Support for @typelet@
-------------------------------------------------------------------------------}

-- | Introduce type variable for a row
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



