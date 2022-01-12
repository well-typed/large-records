{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Record.Anonymous.Internal (
    -- * Types
    Record -- Opaque
  , Field  -- Opaque
  , Merge
    -- * User-visible API
  , empty
  , insert
  , merge
    -- * Convenience functions
  , get
  , set
    -- * Generics
  , RecordConstraints(..)
  , RecordMetadata(..)
    -- * Internal API
  , unsafeRecordHasField
  , unsafeDictRecord
  , unsafeFieldMetadata
  ) where

import Data.Aeson
import Data.Kind
import Data.Map (Map)
import Data.Proxy
import Data.Record.Generic.Eq
import Data.Record.Generic.JSON
import Data.Record.Generic.Show
import Data.SOP.BasicFunctors
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.Records.Compat
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import Data.Record.Generic

import qualified Data.Record.Generic.Rep.Internal as Rep

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XOverloadedLabels
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeApplications
-- >>> :set -fplugin=Data.Record.Anonymous.Plugin
-- >>> :set -dppr-cols=200
-- >>> import GHC.Records.Compat
-- >>> let example :: Record '[ '("a", Bool) ] = insert #a True empty

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Anonymous record
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
-- True
--
-- >>> get #b example
-- ...
-- ...No instance for (HasField "b" (Record...
-- ...
--
-- >>> get #a example :: Int
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
--
-- When part of the record is not known, it might not be possible to resolve a
-- 'HasField' constraint until later. For example, in
--
-- >>> (\r -> get #x r) :: Record '[ '(f, a), '("x", b) ] -> b
-- ...
-- ...No instance for (HasField "x" (...
-- ...
--
-- This is important, because if @f == "x"@, this would only be sound if also
-- @a == b@. We /could/ introduce a new constraint to say precisely that, but
-- it would have little benefit; instead we just leave the 'HasField' constraint
-- unresolved until we know more about the record.
newtype Record (r :: [(Symbol, Type)]) = MkR (Map String Any)

data Field l where
  Field :: KnownSymbol l => Proxy l -> Field l

instance (l ~ l', KnownSymbol l) => IsLabel l' (Field l) where
  fromLabel = Field (Proxy @l)

-- | Result of merging two records
--
-- See 'merge' for details.
type family Merge :: [(Symbol, Type)] -> [(Symbol, Type)] -> [(Symbol, Type)]

{-------------------------------------------------------------------------------
  User-visible API
-------------------------------------------------------------------------------}

-- | Empty record
empty :: Record '[]
empty = MkR Map.empty

-- | Insert a new field into a record
--
-- If a field with this name already exists, the new field will override it.
insert :: Field l -> a -> Record r -> Record ('(l, a) ': r)
insert (Field l) a (MkR r) = MkR $ Map.insert (symbolVal l) (unsafeCoerce a) r

-- | Merge two records
--
-- 'HasField' constraint can be resolved for merged records, subject to the same
-- condition discussed in the documentation of 'Record': since records are left
-- biased, all fields in the record must be known up to the requested field:
--
-- Simple example, completely known record:
--
-- >>> :{
--   let example :: Record (Merge '[ '("a", Bool)] '[ '("b", Char)])
--       example = merge (insert #a True empty) (insert #b 'a' empty)
--   in get #b example
-- :}
-- 'a'
--
-- Slightly more sophisticated, only part of the record known:
--
-- >>> :{
--   let example :: Record (Merge '[ '("a", Bool)] r) -> Bool
--       example = get #a
--   in example (merge (insert #a True empty) (insert #b 'a' empty))
-- :}
-- True
--
-- Rejected example: first part of the record unknown:
--
-- >>> :{
--   let example :: Record (Merge r '[ '("b", Char)]) -> Char
--       example = get #b
--   in example (merge (insert #a True empty) (insert #b 'a' empty))
-- :}
-- ...
-- ...No instance for (HasField "b" (...
-- ...
--
-- TODO: Talk about 'castRecord'.
merge :: Record r -> Record r' -> Record (Merge r r')
merge (MkR r) (MkR r') = MkR $ Map.union r r'

{-------------------------------------------------------------------------------
  Convenience functions

  Defined here for the sake of the docspec examples (these do not actually
  rely on internal API).
-------------------------------------------------------------------------------}

-- | Get record field
--
-- This is a simple wrapper for 'getField'.
get :: forall l r a.
     HasField l (Record r) a
  => Field l -> Record r -> a
get _ = getField @l @(Record r)

-- | Set record field
--
-- This is a simple wrapper for 'setField'.
set :: forall l r a.
     HasField l (Record r) a
  => Field l -> a -> Record r -> Record r
set _ = flip (setField @l @(Record r))


{-------------------------------------------------------------------------------
  Generics
-------------------------------------------------------------------------------}

class RecordMetadata (r :: [(Symbol, Type)]) where
  recordMetadata :: Metadata (Record r)

class RecordMetadata r => RecordConstraints r c where
  dictRecord :: Proxy c -> Rep (Dict c) (Record r)

instance RecordMetadata r => Generic (Record r) where
  type Constraints (Record r) = RecordConstraints r
  type MetadataOf  (Record r) = r

  dict       = dictRecord
  metadata _ = recordMetadata

  -- Implementation note: the order of the fields in the vector must match
  -- the order as specified by the user in the type.
  --
  -- TODO: Can we avoid the O(n log n) cost?

  from :: Record r -> Rep I (Record r)
  from (MkR r) =
      Rep.unsafeFromListAny $ map aux names
    where
      names :: [String]
      names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record r))

      aux :: String -> I Any
      aux nm = case Map.lookup nm r of
                 Just x  -> I x
                 Nothing -> error "impossible: non-existent field"

  to :: Rep I (Record r) -> Record r
  to =
      MkR . Map.fromList . zipWith aux names . Rep.toListAny
    where
      names :: [String]
      names = Rep.collapse $ recordFieldNames $ metadata (Proxy @(Record r))

      aux :: String -> I Any -> (String, Any)
      aux name (I x) = (name, x)

{-------------------------------------------------------------------------------
  Instances

  These could be defined outside of the @Internal@ module: we do not need access
  to the low-level representation. We define them here anyway for two reasons:

  1. Avoid orphans
  2. The doctest examples in this module rely on them.
-------------------------------------------------------------------------------}

instance (RecordConstraints r Show, RecordMetadata r) => Show (Record r) where
  showsPrec = gshowsPrec

instance (RecordConstraints r Eq, RecordMetadata r) => Eq (Record r) where
  (==) = geq

instance ( RecordConstraints r Eq
         , RecordConstraints r Ord
         , RecordMetadata r
         ) => Ord (Record r) where
  compare = gcompare

instance RecordConstraints r ToJSON => ToJSON (Record r) where
  toJSON = gtoJSON

instance RecordConstraints r FromJSON => FromJSON (Record r) where
  parseJSON = gparseJSON

{-------------------------------------------------------------------------------
  Internal API
-------------------------------------------------------------------------------}

-- | Used by the plugin during evidence construction for 'HasField'
--
-- Precondition: the record must have the specified field with type @a@
-- (this precondition is verified by the plugin before generating "evidence"
-- that uses this function).
unsafeRecordHasField :: forall r a. String -> Record r -> (a -> Record r, a)
unsafeRecordHasField label (MkR r) = (
      \a -> MkR $ Map.insert label (unsafeCoerce a) r
    , case Map.lookup label r of
        Just f  -> unsafeCoerce f
        Nothing -> error preconditionViolation
    )
  where
    preconditionViolation :: String
    preconditionViolation = concat [
          "unsafeRecordHasField precondition violation: field "
        , label
        , " not found"
        ]

-- | Used by the plugin during evidence construction for 'RecordConstraints'
unsafeDictRecord :: forall r c.
     [Dict c Any]  -- ^ Dictionary for each field, in order
  -> Proxy c
  -> Rep (Dict c) (Record r)
unsafeDictRecord ds _ = Rep.unsafeFromListAny ds

-- | Used by the plugin during evidence construction for 'RecordMetadata'
unsafeFieldMetadata :: forall r.
     [FieldMetadata Any]
  -> Rep FieldMetadata (Record r)
unsafeFieldMetadata = Rep.unsafeFromListAny