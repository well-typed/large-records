{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

-- | Simple dynamic record type, for testing purposes only
module Test.Infra.DynRecord (
    -- * Definition
    DynRecord(..)
  , Value(..)
    -- * Parsing
  , ParseError
  , FromValue(..)
  , fromValues
    -- * Unparsing
  , ToValue(..)
  , toValues
  ) where

import Data.Bifunctor
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
  Parsing and unparsing
-------------------------------------------------------------------------------}

type ParseError = String

class FromValue (f :: k -> Type) (a :: k) where
  fromValue :: Value -> Either ParseError (f a)

instance FromValue I Int where
  fromValue (VI x) = Right (I x)
  fromValue _      = Left "Expected Int"

instance FromValue I Bool where
  fromValue (VB x) = Right (I x)
  fromValue _      = Left "Expected Bool"

instance FromValue I Char where
  fromValue (VC x) = Right (I x)
  fromValue _      = Left "Expected Char"

fromValues :: forall k (f :: k -> Type) (r :: Row k).
      (KnownFields r, AllFields r (FromValue f))
   => Record (K Value) r
   -> Either ParseError (Record f r)
fromValues r =
    Anon.czipWithM (Proxy @(FromValue f)) aux (Anon.reifyKnownFields r) r
  where
    aux :: FromValue f x => K String x -> K Value x -> Either ParseError (f x)
    aux (K n) (K v) = first (\e -> n ++ ": " ++ e) $ fromValue v

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


