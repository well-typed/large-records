{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs #-}

-- | Simple dynamic record type, for testing purposes only
module Test.Infra.DynRecord (
    -- * Definition
    DynRecord(..)
  , Value(..)
    -- * Unparsing
  , ToValue(..)
  , toValues
  ) where

import Data.Kind

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data DynRecord = DynRecord [(String, Value)]
  deriving (Show, Eq)

data Value =
    VI Int
  | VB Bool
  | VC Char
  deriving (Show, Eq)

{-------------------------------------------------------------------------------
  Unparsing
-------------------------------------------------------------------------------}

class ToValue (f :: k -> Type) (a :: k) where
  toValue :: f a -> Value

instance ToValue I Int  where toValue = VI . unI
instance ToValue I Bool where toValue = VB . unI
instance ToValue I Char where toValue = VC . unI

toValues :: forall k (f :: k -> Type) (r :: Row k).
     AllFields r (ToValue f)
  => Record f r
  -> Record (K Value) r
toValues = Anon.cmap (Proxy @(ToValue f)) aux
  where
    aux :: ToValue f x => f x -> K Value x
    aux = K . toValue
