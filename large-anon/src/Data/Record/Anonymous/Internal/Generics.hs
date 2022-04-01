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
module Data.Record.Anonymous.Internal.Generics () where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Kind
import Data.Record.Generic
import Data.Record.Generic.Eq
import Data.Record.Generic.JSON
import Data.Record.Generic.Show
import Data.SOP
import GHC.Exts (Any)

import qualified Data.Vector as Vector

import Data.Record.Anon.Plugin.Internal.Runtime

import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Constraints

import qualified Data.Record.Anonymous.Internal.Rep as Rep

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
