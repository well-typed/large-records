{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}

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

import Data.Kind
import Data.Map (Map)
import Data.Proxy
import Data.SOP.BasicFunctors
import GHC.Exts (Any)
import GHC.OverloadedLabels
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Map as Map

import Data.Record.Generic

import qualified Data.Record.Generic.Rep.Internal as Rep

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Anonymous record
--
-- To access fields of the record, either use the 'HasFie;d' instances
-- (possibly using the record-dot-preprocessor to get record-dot syntax),
-- or using the simple wrappers 'get' and 'set'. The 'HasField' instances
-- are resolved by the plugin, so be sure to use
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}
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