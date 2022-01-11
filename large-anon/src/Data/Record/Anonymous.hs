{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- See discussion avbout orphans, below.
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Anonymous (
    Record -- Opaque
  , Field  -- Opaque (use @#foo@ to create values)
    -- * Core API
  , empty
  , insert
    -- * Additional convenience functions
  , get
  , set
  , describeRecord
    -- * Generics
  , RecordConstraints(..)
  , RecordMetadata(..)
    -- ** Re-exports
  , module GHC.Records.Compat
  , module Data.Record.Generic
  ) where

import Data.Aeson
import Data.List (intercalate)
import Data.Proxy
import Data.Record.Generic
import Data.Record.Generic.JSON
import Data.Typeable
import GHC.Records.Compat

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Anonymous.Internal

{-------------------------------------------------------------------------------
  Convenience functions
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
  Instances

  Technically these are orphans, but we define them here anyway to keep the
  scope of the internal module as small as possible: we do not need access to
  the low-level details of the record implementation here.
-------------------------------------------------------------------------------}

instance (RecordConstraints r Show, RecordMetadata r) => Show (Record r) where
  show = gshowRecord

instance (RecordConstraints r Eq, RecordMetadata r) => Eq (Record r) where
  (==) = geqRecord

instance ( RecordConstraints r Eq
         , RecordConstraints r Ord
         , RecordMetadata r
         ) => Ord (Record r) where
  compare = gcompareRecord

instance RecordConstraints r ToJSON => ToJSON (Record r) where
  toJSON = gtoJSON

instance RecordConstraints r FromJSON => FromJSON (Record r) where
  parseJSON = gparseJSON

{-------------------------------------------------------------------------------
  Generic functions to support the instances above
-------------------------------------------------------------------------------}

gshowRecord :: forall r. RecordConstraints r Show => Record r -> String
gshowRecord =
      combine
    . Rep.collapse
    . Rep.czipWith (Proxy @Show) (mapKIK aux) names
    . from
  where
    names :: Rep (K String) (Record r)
    names = recordFieldNames $ metadata (Proxy @(Record r))

    aux :: Show x => String -> x -> String
    aux name x = name ++ " = " ++ show x

    combine :: [String] -> String
    combine fs = concat [
          "Record {"
        , intercalate ", " fs
        , "}"
        ]

geqRecord :: RecordConstraints r Eq => Record r -> Record r -> Bool
geqRecord r r' =
      and
    . Rep.collapse
    $ Rep.czipWith (Proxy @Eq) (mapIIK (==)) (from r) (from r')

gcompareRecord :: RecordConstraints r Ord => Record r -> Record r -> Ordering
gcompareRecord r r' =
      mconcat
    . Rep.collapse
    $ Rep.czipWith (Proxy @Ord) (mapIIK compare) (from r) (from r')

{-------------------------------------------------------------------------------
  Addittional functions
-------------------------------------------------------------------------------}

-- | Show type of every field in the record
describeRecord :: forall proxy r.
     RecordConstraints r Typeable
  => proxy r  -- Proxy for the record (note: can use a record itself as a proxy)
  -> String
describeRecord _ =
      combine
    . Rep.collapse
    . Rep.cmap (Proxy @Typeable) aux
    $ names
  where
    names :: Rep (K String) (Record r)
    names = recordFieldNames $ metadata (Proxy @(Record r))

    aux :: forall x. Typeable x => K String x -> K String x
    aux (K name) = K $ name ++ " :: " ++ show (typeRep (Proxy @x))

    combine :: [String] -> String
    combine fs = concat [
          "Record {"
        , intercalate ", " fs
        , "}"
        ]

