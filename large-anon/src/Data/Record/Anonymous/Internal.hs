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
    -- * User-visible API
  , empty
  , insert
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
-- >>> import GHC.Records.Compat
-- >>> :set -fplugin=Data.Record.Anonymous.Plugin
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
-- Some examples, using
--
-- > example :: Record '[ '("a", Bool) ]
-- > example = insert #a True empty
--
-- >>> getField @"a" example -- or @example.a@ if using RecordDotSyntax
-- True
--
-- >>> getField @"b" example
-- ...
-- ...No instance for (HasField "b" (Record...
-- ...
--
-- >>> getField @"a" example :: Int
-- ...
-- ...Couldn't match...Int...Bool...
-- ...
newtype Record (r :: [(Symbol, Type)]) = MkR (Map String Any)

data Field l where
  Field :: KnownSymbol l => Proxy l -> Field l

instance (l ~ l', KnownSymbol l) => IsLabel l' (Field l) where
  fromLabel = Field (Proxy @l)

{-------------------------------------------------------------------------------
  User-visible API
-------------------------------------------------------------------------------}

empty :: Record '[]
empty = MkR Map.empty

insert :: Field l -> a -> Record r -> Record ('(l, a) ': r)
insert (Field l) a (MkR r) = MkR $ Map.insert (symbolVal l) (unsafeCoerce a) r

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