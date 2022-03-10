{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Record.Generic (
    -- * Types with a generic view
    Generic(..)
  , Rep(..) -- TODO: Make opaque?
    -- * Metadata
  , Metadata(..)
  , FieldStrictness(..)
  , recordFieldNames
  , FieldMetadata(..)
    -- * Working with type-level metadata
  , FieldName
  , FieldType
  , IsField
    -- * Re-exports
  , module SOP
  , Proxy(..)
  ) where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

-- To reduce overlap between the two libraries and improve interoperability,
-- we import as much from sop-core as possible.
import Data.SOP.BasicFunctors as SOP
import Data.SOP.Classes       as SOP (type (-.->)(..))
import Data.SOP.Dict          as SOP (Dict(..))

import Data.Record.Generic.Rep.Internal (Rep(..))

import qualified Data.Record.Generic.Rep.Internal as Rep

{-------------------------------------------------------------------------------
  Generic type class
-------------------------------------------------------------------------------}

class Generic a where
  -- | @Constraints a c@ means "all fields of @a@ satisfy @c@"
  type Constraints a :: (Type -> Constraint) -> Constraint

  -- | Type-level metadata
  --
  -- NOTE: using type-level lists without resulting in quadratic core code is
  -- extremely difficult. Any use of this type-level metadata therefore needs
  -- delibrate consideration. Some examples:
  --
  -- o Within the @large-generics@ library, 'MetadataOf' is used in the
  --   definition of 'HasNormalForm'. This constraint is carefully defined to
  --   avoid quadratic code, as described in the presentation
  --   "Avoiding Quadratic Blow-up During Compilation"
  --   <https://skillsmatter.com/skillscasts/17262-avoiding-quadratic-blow-up-during-compilation>
  -- o The @large-records@ library uses it to provide a compatibility layer
  --   between it and @sop-core@; this is however only for testing purposes, and
  --   the quadratic code here is simply accepted.
  type MetadataOf a :: [(Symbol, Type)]

  -- | Translate to generic representation
  from :: a -> Rep I a

  -- | Translate from generic representation
  to :: Rep I a -> a

  -- | Construct vector of dictionaries, one for each field of the record
  dict :: Constraints a c => Proxy c -> Rep (Dict c) a

  -- | Metadata
  metadata :: proxy a -> Metadata a

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

data Metadata a = Metadata {
      recordName          :: String
    , recordConstructor   :: String
    , recordSize          :: Int
    , recordFieldMetadata :: Rep FieldMetadata a
    }

data FieldStrictness = FieldStrict | FieldLazy

data FieldMetadata x where
  FieldMetadata ::
       KnownSymbol name
    => Proxy name
    -> FieldStrictness
    -> FieldMetadata x

recordFieldNames :: Metadata a -> Rep (K String) a
recordFieldNames = Rep.map' aux . recordFieldMetadata
  where
    aux :: FieldMetadata x -> K String x
    aux (FieldMetadata p _) = K $ symbolVal p

{-------------------------------------------------------------------------------
  Working with the type-level metadata
-------------------------------------------------------------------------------}

type family FieldName (field :: (Symbol, Type)) :: Symbol where
  FieldName '(name, _typ) = name

type family FieldType (field :: (Symbol, Type)) :: Type where
  FieldType '(_name, typ) = typ

class (field ~ '(FieldName field, FieldType field)) => IsField field
instance (field ~ '(FieldName field, FieldType field)) => IsField field
