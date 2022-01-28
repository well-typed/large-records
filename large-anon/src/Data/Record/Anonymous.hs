{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
    -- * Combinators
    -- ** "Functor"
  , map
  , mapM
  , cmap
  , cmapM
    -- ** Zipping
  , zip
  , zipWith
  , zipWithM
  , czipWith
  , czipWithM
    -- ** "Foldable"
  , collapse
    -- ** "Traversable"
  , sequenceA
    -- ** "Applicative"
  , pure
  , cpure
  , ap
    -- * Generics
    -- ** Metadata
  , RecordMetadata -- opaque
  , recordMetadata
  , RecordMetadataOf
    -- ** Constraints
  , RecordDicts(..)
  , RecordDictsF
  , recordDictsF
  , RecordConstraints(..)
    -- ** Re-exports
  , module GHC.Records.Compat
  , module Data.Record.Generic
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)

import Data.List (intercalate)
import Data.Proxy
import Data.Record.Generic
import Data.SOP (fn_2)
import Data.Typeable
import GHC.Records.Compat

import qualified Data.Record.Generic.Rep as Rep

import Data.Record.Anonymous.Internal

{-------------------------------------------------------------------------------
  Constrained combinators

  These are defined here rather than in Internal because these do not use
  the internal representation directly (but instead are defined in terms of the
  non-constrained combinators).
-------------------------------------------------------------------------------}

cpure ::
     RecordDicts r c
  => Proxy c
  -> (forall x. c x => f x)
  -> Record f r
cpure p f = map (\Dict -> f) (recordDicts p)

cmap :: forall f g r c.
     RecordDicts r c
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> Record f r -> Record g r
cmap p f = ap (cpure p (Fn f))

cmapM ::
     (Applicative m, RecordDicts r c)
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> Record f r -> m (Record g r)
cmapM p f = sequenceA . cmap p (Comp . f)

czipWithM ::
     (Applicative m, RecordDicts r c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
czipWithM p f a b = sequenceA $
    cpure p (fn_2 $ \x y -> Comp $ f x y) `ap` a `ap` b

czipWith ::
     RecordDicts r c
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
czipWith p f a b = unI $ czipWithM p (\x y -> I (f x y)) a b

{-------------------------------------------------------------------------------
  Additional functions
-------------------------------------------------------------------------------}

-- | Show type of every field in the record
describeRecord :: forall proxy f r.
     RecordConstraints f r Typeable
  => proxy f r  -- Proxy for the record (note: can use a record itself as a proxy)
  -> String
describeRecord _ =
      combine
    . Rep.collapse
    . Rep.cmap (Proxy @Typeable) aux
    $ names
  where
    names :: Rep (K String) (Record f r)
    names = recordFieldNames $ metadata (Proxy @(Record f r))

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

