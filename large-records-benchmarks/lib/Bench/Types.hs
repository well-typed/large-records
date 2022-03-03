{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Bench.Types (
    T(..)
  , HK(..)
  ) where

import Data.Aeson
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Kind
import GHC.TypeLits

import Data.Record.Generic
import Data.Record.Generic.LowerBound
import Data.Record.Generic.Transform

-- | 'T' gives us as many different types as we need
newtype T (i :: Nat) = MkT Word
  deriving (Show, Eq, ToJSON)

type instance Interpreted I (T i) = T i

-- | Like 'T', but with a higher-kinded type variable
newtype HK (i :: Nat) (f :: Type -> Type) = MkHK (f Word)

instance KnownNat i => LowerBound (T i) where
  lowerBound = MkT $ fromInteger $ natVal (Proxy @i)

instance KnownNat i => LowerBound (HK i Identity) where
  lowerBound = MkHK $ fromInteger $ natVal (Proxy @i)

instance Show1 f => Show (HK i f) where
  showsPrec d (MkHK x) = showsPrec1 d x
