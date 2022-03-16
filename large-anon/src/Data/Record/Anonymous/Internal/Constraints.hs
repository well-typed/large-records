{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , recordOfDicts
  ) where

import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Dict
import GHC.Exts (Any)

import Data.Vector.Generic as V

import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Canonical          as Canon
import qualified Data.Record.Anonymous.Internal.Combinators.Simple as Simple
import qualified Data.Record.Anonymous.Internal.Record             as Record

{-------------------------------------------------------------------------------
  Reifiying dictionaries
-------------------------------------------------------------------------------}

data Constrained c f x where
  Constrained :: c x => f x -> Constrained c f x

constrain :: forall c f r.
      AllFields r c
   => Proxy c -> Record f r -> Record (Constrained c f) r
constrain p (Record.canonicalize -> r) = Record.unsafeFromCanonical $
    Canon.fromLazyVector $
      V.zipWith aux (Canon.toLazyVector r) (fieldDicts (Proxy @r) p)
  where
    aux :: f Any -> Dict c Any -> Constrained c f Any
    aux x Dict = Constrained x

{-------------------------------------------------------------------------------
  Taking the functor into account
-------------------------------------------------------------------------------}

class    (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c
instance (AllFields r (Compose c f), KnownFields r) => RecordConstraints f r c

recordOfDicts :: forall f r c.
     RecordConstraints f r c
  => Proxy c -> Record (Dict c :.: f) r
recordOfDicts _ =
    Simple.map aux $ constrain (Proxy @(Compose c f)) (Simple.pure (K ()))
  where
    aux :: Constrained (Compose c f) (K ()) x -> (:.:) (Dict c) f x
    aux (Constrained _) = Comp Dict
