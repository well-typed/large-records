{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Conversion between 'Record' and @large-generics@ 'Rep'
--
-- Intended for qualified import.
module Data.Record.Anonymous.Internal.Rep (
    -- * Conversions
    fromRecord
  , toRecord
  , fromRecord'
  , toRecord'
  ) where

import Data.Kind
import Data.Record.Generic.Rep.Internal (Rep(..), noInlineUnsafeCo)
import Data.SOP.BasicFunctors
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import qualified Data.Vector as Lazy

import Data.Record.Anonymous.Internal.Record (Record)

import qualified Data.Record.Anonymous.Internal.Canonical as Canon
import qualified Data.Record.Anonymous.Internal.Record    as Record

{-------------------------------------------------------------------------------
  Normalize 'Rep'

  This is similar in spirit to the normalization infrastructure in
  @large-generics@, but do not depend on any 'HasNormalForm' constraint.

  TODO: These types aren't quite as polymorphic as one might like; not sure
  what the right generalization is. For now these suffice.
-------------------------------------------------------------------------------}

denormalize :: forall k
                      (f :: Type -> Type)
                      (g :: k -> Type)
                      (r :: [(Symbol, k)]).
  Rep I (Record (f :.: g) r) -> Rep f (Record g r)
denormalize (Rep r) = Rep (co r)
  where
    -- The first  @Any@ is really @(f (g Any))@
    -- The second @Any@ is really @(g Any)@
    co :: Lazy.Vector (I Any) -> Lazy.Vector (f Any)
    co = noInlineUnsafeCo

normalize :: forall f g r.
  Rep f (Record g r) -> Rep I (Record (f :.: g) r)
normalize (Rep r) = Rep (co r)
  where
    -- The first  @Any@ is really @(g Any)@
    -- The second @Any@ is really @(f (g Any))@
    co :: Lazy.Vector (f Any) -> Lazy.Vector (I Any)
    co = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

fromRecord :: Record f r -> Rep I (Record f r)
fromRecord (Record.canonicalize -> r) =
    Rep $ co . Canon.toLazyVector $ r
  where
    -- Second @Any@ is really (f (Any))
    co :: Lazy.Vector (f Any) -> Lazy.Vector (I Any)
    co = noInlineUnsafeCo

toRecord :: Rep I (Record f r) -> Record f r
toRecord (Rep r) =
    Record.unsafeFromCanonical $ Canon.fromLazyVector . co $ r
  where
    -- First @Any@ is really (f Any)@
    co :: Lazy.Vector (I Any) -> Lazy.Vector (f Any)
    co = noInlineUnsafeCo

fromRecord' :: Record (f :.: g) r -> Rep f (Record g r)
fromRecord' = denormalize . fromRecord

toRecord' :: Rep f (Record g r) -> Record (f :.: g) r
toRecord' = toRecord . normalize
