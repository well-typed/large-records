{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Proof of concept: mark strictness at the type-level
--
-- This is an illustration of how to take advantage of the polykinded nature of
-- the advanced 'Record' interface.
module Test.Infra.MarkStrictness (
    -- * Definition
    MarkStrictness(..)
  , Boxed(..)
  ) where

import Data.Kind
import Data.SOP.BasicFunctors

import Data.Record.Anon

import Test.Infra.DynRecord
import Test.Infra.DynRecord.Advanced

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data MarkStrictness a = Strict a | Lazy a

data Boxed :: MarkStrictness Type -> Type where
  BoxStrict :: !a -> Boxed (Strict a)
  BoxLazy   ::  a -> Boxed (Lazy   a)

{-------------------------------------------------------------------------------
  Standard instances
-------------------------------------------------------------------------------}

instance Show a => Show (Boxed (Strict a)) where
  show (BoxStrict x) = show x
instance Show a => Show (Boxed (Lazy a)) where
  show (BoxLazy x) = show x

instance Eq a => Eq (Boxed (Strict a)) where
  BoxStrict x == BoxStrict y = x == y
instance Eq a => Eq (Boxed (Lazy a)) where
  BoxLazy x == BoxLazy y = x == y

{-------------------------------------------------------------------------------
  Interop with 'DynRecord'
-------------------------------------------------------------------------------}

instance ToValue I a => ToValue Boxed (Lazy a) where
  toValue (BoxLazy x) = toValue (I x)
instance ToValue I a => ToValue Boxed (Strict a) where
  toValue (BoxStrict x) = toValue (I x)

-- | Type inference for a value
--
-- Just for the example, we infer all 'Int' fields are strict and all other
-- fields as lazy.
instance IsValue Boxed where
  isValue (VI x) = Some $ ValidField $ BoxStrict x
  isValue (VB x) = Some $ ValidField $ BoxLazy   x
  isValue (VC x) = Some $ ValidField $ BoxLazy   x
