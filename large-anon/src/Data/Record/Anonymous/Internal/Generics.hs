{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Integration with @large-generics@
module Data.Record.Anonymous.Internal.Generics (
    -- * Additional generic functions
    describeRecord
  , recordWithMetadata
  , recordWithNames
    -- * Debugging
  , debugFieldTypes
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Kind
import Data.List (intercalate)
import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.JSON
import Data.Record.Generic.Show
import Data.SOP
import Data.Typeable
import GHC.Exts (Any)
import GHC.TypeLits

import qualified Data.Record.Generic.Rep as Rep
import qualified Data.Vector             as Vector

import Data.Record.Anon.Plugin.Internal.Runtime

import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Constraints

import qualified Data.Record.Anonymous.Internal.Rep as Rep
import qualified Data.Record.Anonymous.Internal.Combinators.Simple as Simple

{-------------------------------------------------------------------------------
  Integrate with large-generics
-------------------------------------------------------------------------------}

recordConstraints :: forall f r c.
     RecordConstraints f r c
  => Proxy c -> Rep (Dict c) (Record f r)
recordConstraints _ = Rep $
    Vector.map (co . aux) $ fieldDicts (Proxy @r) (Proxy @(Compose c f))
  where
    aux :: DictAny (Compose c f) -> Dict (Compose c f) Any
    aux DictAny = Dict

    -- The second 'Any' is really (f Any)
    co :: Dict (Compose c f) Any -> Dict c Any
    co = noInlineUnsafeCo

recordMetadata :: forall k (f :: k -> Type) (r :: Row k).
     KnownFields r
  => Metadata (Record f r)
recordMetadata = Metadata {
      recordName          = "Record"
    , recordConstructor   = "Record"
    , recordSize          = length fields
    , recordFieldMetadata = Rep $ Vector.fromList fields
    }
  where
    fields :: [FieldMetadata Any]
    fields = fieldMetadata (Proxy @r)

instance KnownFields r => Generic (Record f r) where
  type Constraints (Record f r) = RecordConstraints f r
  type MetadataOf  (Record f r) = FieldTypes        f r

  from     = Rep.fromRecord
  to       = Rep.toRecord
  dict     = recordConstraints
  metadata = const recordMetadata

{-------------------------------------------------------------------------------
  Instances for standard type classes

  These instances all depend on the generics integration.
-------------------------------------------------------------------------------}

instance RecordConstraints f r Show => Show (Record f r) where
  showsPrec = gshowsPrec

instance RecordConstraints f r Eq => Eq (Record f r) where
  (==) = geq

instance ( RecordConstraints f r Eq
         , RecordConstraints f r Ord
         ) => Ord (Record f r) where
  compare = gcompare

instance RecordConstraints f r ToJSON => ToJSON (Record f r) where
  toJSON = gtoJSON

instance RecordConstraints f r FromJSON => FromJSON (Record f r) where
  parseJSON = gparseJSON

{-------------------------------------------------------------------------------
  Additional functions
-------------------------------------------------------------------------------}

-- | Show type of every field in the record
describeRecord :: forall (a :: Type).
     (Generic a, Constraints a Typeable)
  => Proxy a
  -> String
describeRecord p =
      combine
    . Rep.collapse
    . Rep.cmap (Proxy @Typeable) aux
    $ names
  where
    names :: Rep (K String) a
    names = recordFieldNames $ metadata p

    -- @x@ here will be of the form @f x'@, for some @x'@, and we have a
    -- constraint @Typeable (f x')@ in scope. We therefore do not need to
    -- manually apply @f@ here.
    aux :: forall x. Typeable x => K String x -> K String x
    aux (K name) = K $ name ++ " :: " ++ show (typeRep (Proxy @x))

    combine :: [String] -> String
    combine fs = concat [
          "Record {"
        , intercalate ", " fs
        , "}"
        ]

-- | Construct record with field metadata for every field
recordWithMetadata :: forall k (f :: k -> Type) (r :: Row k) proxy.
     KnownFields r
  => proxy r -> Record (FieldMetadata :.: f) r
recordWithMetadata _ =
    Rep.toRecord' md
  where
    md :: Rep FieldMetadata (Record f r)
    md = recordFieldMetadata (metadata (Proxy @(Record f r)))

-- | Like 'recordWithMetadata', but includes field names only
recordWithNames :: forall k (r :: Row k) proxy.
     KnownFields r
  => proxy r -> Record (K String) r
recordWithNames _ =
    Simple.map aux $ Rep.toRecord' md
  where
    md :: Rep (K String) (Record (K ()) r)
    md = recordFieldNames (metadata (Proxy @(Record (K ()) r)))

    aux :: (K String :.: f) x -> K String x
    aux (Comp (K name)) = K name

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Like 'describeRecord', but exclusively using type-level information.
--
-- WARNING: The @All@ constraint will lead to quadratic code. This is for
-- debugging only.
debugFieldTypes :: forall f r.
     All IsField (FieldTypes f r)
  => Proxy (Record f r) -> String
debugFieldTypes _ =
    (\str -> "[" ++ str ++ "]") . intercalate "," . hcollapse $
      aux (shape :: Shape (FieldTypes f r))
  where
    aux :: forall fs. All IsField fs => Shape fs -> NP (K String) fs
    aux ShapeNil      = Nil
    aux (ShapeCons s) = name :* aux s

    name :: forall n a. KnownSymbol n => K String '(n, a)
    name = K (symbolVal (Proxy @n))

