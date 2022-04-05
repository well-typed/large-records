{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Runtime for code generated by the plugin
--
-- Users should not need to import from this module directly.
module Data.Record.Anon.Plugin.Internal.Runtime (
    -- * Row
    Pair(..)
  , Row
    -- * RowHasField
  , RowHasField(..)
  , DictRowHasField
  , evidenceRowHasField
    -- * Term-level metadata
  , KnownFields(..)
  , DictKnownFields
  , evidenceKnownFields
  , fieldMetadata
    -- * Type-level metadata
  , FieldTypes
  , SimpleFieldTypes
    -- * AllFields
  , AllFields(..)
  , DictAny(..)
  , DictAllFields
  , evidenceAllFields
    -- * KnownHash
  , KnownHash(..)
  , evidenceKnownHash
    -- * Merging
  , Merge
    -- * Subrecords
  , Project(..)
  , DictProject
  , evidenceProject
    -- * Utility
  , noInlineUnsafeCo
  ) where

import Data.Kind
import Data.Record.Generic hiding (FieldName)
import Data.SOP.Constraint (Compose)
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as Lazy
import qualified Data.Vector as Vector

{-------------------------------------------------------------------------------
  IMPLEMENTATION NOTE

  Support for name resolution in typechecker plugins is a bit rudimentary. The
  only available API is

  > lookupOrig :: Module -> OccName -> TcPluginM Name

  This function can /only/ be used to look things up in the given module that
  are /defined by/ that module; it won't find anything that is merely /exported/
  by the module. This makes name lookup brittle: an internal re-organization
  that changes where things are defined might break the plugin, even if the
  export lists of those modules have not changed. This is merely annoying for
  internal reshuffling, but worse for external reshuffling as such changes would
  be considered entirely backwards compatible and not require any major version
  changes.

  We address this in two ways:

  1. Anything defined internally in this package that needs to be referred by the
     plugin is defined in here in this @.Runtime@ module. This does have the
     unfortunate consequence that this module contains definitions that are not
     necessarily related to each other, apart from "required by the plugin".
  2. We avoid dependencies on external packages altogether. For example, instead
     of the plugin providing evidence for 'HasField' directly, it instead
     provides evidence for a 'HasField'-like class defined here in the
     @.Runtime@ module. We then give a "forwarding" instance for the " real "
     'HasField' in terms of that class; the plugin does not need to be aware of
     that forwarding instance, of course, and it won't be done in this module.

  Avoiding any external dependencies here has an additional advantage: even if
  we accept that the plugin must specifiy the exact module where something is
  defined in an external package, there is a secondary problem: users who then
  use the plugin must declare those packages as explicit dependencies, or else
  name resolution will fail at compile time (of the user's package) with a
  mysterious error message. It may be possible to work around this problem by
  using something else instead of @findImportedModule@, but avoiding external
  dependencies just bypasses the problem altogether.

  NOTE: In order to avoid headaches with cyclic module dependencies, we use the
  convention that the runtime can only import from @Data.Record.Anon.Internal.Core.*@,
  which in turn cannot import from the runtime (and can only import from other
  modules in the Core.*@). One important consequence of this split is that
  nothing in @Core.*@ is aware of the concept of rows, which is introduced here.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Row
-------------------------------------------------------------------------------}

-- | Pair of values
--
-- This is used exclusively promoted to the type level, in 'Row'.
data Pair a b = a := b

-- | Row: type-level list of field names and corresponding field types
type Row k = [Pair Symbol k]

{-------------------------------------------------------------------------------
  HasField
-------------------------------------------------------------------------------}

-- | Specialized form of 'HasField'
--
-- @RowHasField n r a@ holds if there is an @(n := a)@ in @r@.
--
-- TODO: Record now no longer needs to live in Core?
class RowHasField (n :: Symbol) (r :: Row k) (a :: k) | n r -> a where
  rowHasField :: DictRowHasField k n r a
  rowHasField = undefined

type DictRowHasField k (n :: Symbol) (r :: Row k) (a :: k) =
   Proxy n -> Proxy r -> Proxy a -> Int

-- | Evidence for 'HasField'
--
-- The evidence is simply the index of the field.
evidenceRowHasField :: Int -> DictRowHasField k n r a
evidenceRowHasField i _ _ _ = i

{-------------------------------------------------------------------------------
  Term-level metadata

  NOTE: Here and elsewhere, we provide an (undefined) default implementation,
  to avoid the method showing up in the Haddocks. In practice this makes no
  difference: the body of the class is not exported, and instances are instead
  computed by the plugin.
-------------------------------------------------------------------------------}

-- | Require that all field names in @r@ are known
class KnownFields (r :: Row k) where
  fieldNames :: DictKnownFields k r
  fieldNames = undefined

type DictKnownFields k (r :: Row k) = Proxy r -> [String]

evidenceKnownFields :: forall k (r :: Row k).
  [String] -> DictKnownFields k r
evidenceKnownFields x _ = x

{-------------------------------------------------------------------------------
  Type-level metadata
-------------------------------------------------------------------------------}

-- | Type-level metadata
--
-- >    FieldTypes Maybe [ "a" := Int, "b" := Bool ]
-- > == [ '("a", Maybe Int), '("b", Maybe Bool) ]
type family FieldTypes (f :: k -> Type) (r :: Row k) :: [(Symbol, Type)]

-- | Like 'FieldTypes', but for the simple API (no functor argument)
--
-- >    SimpleFieldTypes [ "a" := Int, "b" := Bool ]
-- > == [ '("a", Int), '("b", Bool) ]
type family SimpleFieldTypes (r :: Row Type) :: [(Symbol, Type)]

{-------------------------------------------------------------------------------
  AllFields
-------------------------------------------------------------------------------}

-- | Require that @c x@ holds for every @(n := x)@ in @r@.
class AllFields (r :: Row k) (c :: k -> Constraint) where
  -- | Vector of dictionaries, in row order
  --
  -- This returns a /lazy/ vector because it is used to build a 'Rep'.
  fieldDicts :: DictAllFields k r c
  fieldDicts = undefined

type DictAllFields k (r :: Row k) (c :: k -> Constraint) =
       Proxy r -> Proxy c -> Lazy.Vector (DictAny c)

data DictAny c where
  DictAny :: c Any => DictAny c

evidenceAllFields :: forall k (r :: Row k) (c :: k -> Constraint).
  [DictAny c] -> DictAllFields k r c
evidenceAllFields x _ _ = Vector.fromList x

instance {-# OVERLAPPING #-}
         (KnownFields r, Show a)
      => AllFields r (Compose Show (K a)) where
  fieldDicts pr _ = Lazy.fromList $ map (const DictAny) (fieldNames pr)

instance {-# OVERLAPPING #-}
         (KnownFields r, Eq a)
      => AllFields r (Compose Eq (K a)) where
  fieldDicts pr _ = Lazy.fromList $ map (const DictAny) (fieldNames pr)

instance {-# OVERLAPPING #-}
         (KnownFields r, Ord a)
      => AllFields r (Compose Ord (K a)) where
  fieldDicts pr _ = Lazy.fromList $ map (const DictAny) (fieldNames pr)

fieldMetadata :: forall k (r :: Row k) proxy.
     KnownFields r
  => proxy r -> [FieldMetadata Any]
fieldMetadata _ = map aux $ fieldNames (Proxy @r)
  where
    -- @large-anon@ only supports records with strict fields.
    aux :: String -> FieldMetadata Any
    aux name = case someSymbolVal name of
                 SomeSymbol p -> FieldMetadata p FieldStrict

{-------------------------------------------------------------------------------
  Merging records
-------------------------------------------------------------------------------}

-- | Merge two rows
--
-- See 'Data.Record.Anon.Advanced.merge' for detailed discussion.
type family Merge :: Row k -> Row k -> Row k

{-------------------------------------------------------------------------------
  KnownHash

  This class is exported /with/ its body from the library (no reason not to).
  so we avoid using 'DictKnownHash' in the class definition.
-------------------------------------------------------------------------------}

-- | Symbol (type-level string) with compile-time computed hash
--
-- Instances are computed on the fly by the plugin.
class KnownHash (s :: Symbol) where
  hashVal :: forall proxy. proxy s -> Int

type DictKnownHash (s :: Symbol) =
       forall proxy. proxy s -> Int

evidenceKnownHash :: forall (s :: Symbol).
  Int -> DictKnownHash s
evidenceKnownHash x _ = x

{-------------------------------------------------------------------------------
  Subrecord
-------------------------------------------------------------------------------}

-- | Subrecords
--
-- If @Project r r'@ holds, we can project (or create a lens) @r@ to @r'@.
-- See 'Data.Record.Anon.Advanced.project' for detailed discussion.
class Project (r :: Row k) (r' :: Row k) where
  projectIndices :: DictProject k r r'
  projectIndices = undefined

-- | In order of the fields in the /target/ record, the index in the /source/
type DictProject k (r :: Row k) (r' :: Row k) =
       Proxy r -> Proxy r' -> [Int]

evidenceProject :: forall k (r :: Row k) (r' :: Row k).
  [Int] -> DictProject k r r'
evidenceProject x _ _ = x

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

-- TODO: It might be nice if we could use this throughout, and avoid using the
-- one from LR.
noInlineUnsafeCo :: a -> b
{-# NOINLINE noInlineUnsafeCo #-}
noInlineUnsafeCo = unsafeCoerce
