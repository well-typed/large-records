{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Simple dynamic record type, for testing purposes only
--
-- Intended for qualified import
--
-- > import Test.Sanity.Existential.DynRecord (DynRecord(..), Parse)
-- > import qualified Test.Sanity.Existential.DynRecord as Dyn
module Test.Sanity.Existential.DynRecord (
    DynRecord(..)
  , Value(..)
  , Parse(..)
  , inferType
  , parse
  , parseSimple
  ) where

import Data.Bifunctor
import Data.Kind
import Data.Record.Generic
import Data.SOP

import Data.Record.Anonymous.Advanced (Record, KnownFields, AllFields, Row)
import Data.Record.Anonymous.Existential

import qualified Data.Record.Anonymous.Advanced as Anon
import qualified Data.Record.Anonymous.Simple   as Simple

{-------------------------------------------------------------------------------
  Dynamically typed record
-------------------------------------------------------------------------------}

data DynRecord = DynRecord [(String, Value)]

data Value = VI Int | VB Bool

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

class Parse (f :: k -> Type) (a :: k) where
  parseField :: Value -> Maybe (f a)

instance Parse I Int where
  parseField (VI x) = Just (I x)
  parseField _      = Nothing

instance Parse I Bool where
  parseField (VB x) = Just (I x)
  parseField _      = Nothing

{-------------------------------------------------------------------------------
  Type discovery and parsing
-------------------------------------------------------------------------------}

inferType :: forall k (cs :: [k -> Constraint]).
     SListI cs
  => SomeField cs -- ^ For 'Int' fields
  -> SomeField cs -- ^ For 'Bool' fields
  -> DynRecord -> SomeFields cs
inferType fInt fBool (DynRecord xs) =
    someFields $ map (second aux) xs
  where
    aux :: Value -> SomeField cs
    aux (VI _) = fInt
    aux (VB _) = fBool

parse :: forall k (f :: k -> Type) (r :: Row k).
     (KnownFields r, AllFields r (Parse f))
  => DynRecord -> Maybe (Record f r)
parse (DynRecord xs) =
    Anon.cmapM (Proxy @(Parse f)) getField $
      Anon.recordWithNames (Proxy @(Record f r))
  where
    getField :: Parse f x => K String x -> Maybe (f x)
    getField (K name) = do
        v <- lookup name xs
        parseField v

parseSimple ::
     (KnownFields r, AllFields r (Parse I))
  => DynRecord -> Maybe (Simple.Record r)
parseSimple = fmap Simple.fromAdvanced . parse