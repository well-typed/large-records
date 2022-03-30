{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Higher-level API on top of 'AllFields'
--
-- Intended for unqualified import.
module Data.Record.Anonymous.Internal.Constraints (
    -- * Reifying dictionaries
    Constrained(..)
  , constrain
    -- * Taking the functor into account
  , RecordConstraints
  , reifyAllFields
  ) where

import Data.Kind
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Dict
import GHC.Exts (Any)

import Data.Vector.Generic as V

import Data.Record.Anon.Plugin.Internal.Runtime

import qualified Data.Record.Anon.Core.Canonical as Canon

import Data.Record.Anonymous.Internal.Record (Record)

import qualified Data.Record.Anonymous.Internal.Record as Record

{-------------------------------------------------------------------------------
  Reifiying dictionaries
-------------------------------------------------------------------------------}

data Constrained (c :: k -> Constraint) (f :: k -> Type) (x :: k) where
  Constrained :: c x => f x -> Constrained c f x

constrain :: forall k (c :: k -> Constraint) (f :: k -> Type) (r :: Row k).
      AllFields r c
   => Proxy c -> Record f r -> Record (Constrained c f) r
constrain p (Record.toCanonical -> r) = Record.unsafeFromCanonical $
    Canon.fromLazyVector $
      V.zipWith aux (Canon.toLazyVector r) (fieldDicts (Proxy @r) p)
  where
    aux :: f Any -> DictAny c -> Constrained c f Any
    aux x DictAny = Constrained x

{-------------------------------------------------------------------------------
  Taking the functor into account
-------------------------------------------------------------------------------}

class    (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c
instance (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c

reifyAllFields :: forall k l (f :: k -> l) (r :: Row k) (c :: l -> Constraint) proxy.
     AllFields r (Compose c f)
  => proxy c -> Record (Dict c :.: f) r
reifyAllFields _ = Record.unsafeFromCanonical $
    Canon.fromLazyVector $
      V.map aux $ fieldDicts (Proxy @r) (Proxy @(Compose c f))
  where
    aux :: DictAny (Compose c f) -> (:.:) (Dict c) f Any
    aux DictAny = Comp Dict

