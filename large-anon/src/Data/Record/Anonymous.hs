{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- See discussion avbout orphans, below.
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Record.Anonymous (
    Record -- Opaque
  , Field  -- Opaque (use @#foo@ to create values)
  , Merge
  , Isomorphic
    -- * Core API
  , empty
  , insert
  , merge
  , castRecord
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

import Data.List (intercalate)
import Data.Proxy
import Data.Record.Generic
import Data.Typeable
import GHC.Records.Compat

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Anonymous.Internal

{-------------------------------------------------------------------------------
  Additional functions
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

