{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Conversion between 'Record' and @large-generics@ 'Rep'
--
-- Intended for qualified import.
module Data.Record.Anonymous.Internal.Rep (
    -- * Lens-like API
    with
  , withNorm
  , with'
    -- * Conversions
  , fromRecord
  , fromRecordNorm
  , fromRecord'
  , toRecord
  , toRecordNorm
  , toRecord'
  ) where

import Data.Proxy
import Data.Record.Generic.Rep.Internal (Rep(..), noInlineUnsafeCo)
import Data.SOP.BasicFunctors
import GHC.Exts (Any)

import qualified Data.Vector as Lazy

import Data.Record.Anonymous.Internal.AfterUnI
import Data.Record.Anonymous.Internal.Canonical (Canonical(..))
import Data.Record.Anonymous.Internal.Record (Record)
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Canonical    as Canon
import qualified Data.Record.Anonymous.Internal.Record       as Record
import qualified Data.Record.Anonymous.Internal.StrictVector as Strict

{-------------------------------------------------------------------------------
  Lens-like API
-------------------------------------------------------------------------------}

with' :: forall f g r a.
     Record (f :.: g) r
  -> (    Rep f (Record g r)
       -> (forall f' g'. Rep f' (Record g' r) -> Record (f' :.: g') r)
       -> a
     )
  -> a
with' (Record.canonicalize -> r) k =
    k rep fromRep
  where
    rep :: Rep f (Record g r)
    rep = Rep $ co (Strict.toLazy (canonValues r))

    fromRep :: forall f' g'. Rep f' (Record g' r) -> Record (f' :.: g') r
    fromRep (Rep values) = Record.unsafeFromCanonical $
         r { canonValues = Strict.fromLazy (co' values) }

    co  :: forall f' g'. Lazy.Vector ((f' :.: g') Any) -> Lazy.Vector (f' Any)
    co' :: forall f' g'. Lazy.Vector (f' Any) -> Lazy.Vector ((f' :.: g') Any)

    co  = noInlineUnsafeCo
    co' = noInlineUnsafeCo

with :: forall f r a.
     Record f r
  -> (    Rep I (Record f r)
       -> (forall f'. Rep I (Record f' r) -> Record f' r)
       -> a
     )
  -> a
with r k =
    with' (co r) $ \rep fromRep ->
      k rep (co' . fromRep)
  where
    co  :: forall f'. Record f' r -> Record (I :.: f') r
    co' :: forall f'. Record (I :.: f') r -> Record f' r

    co  = noInlineUnsafeCo
    co' = noInlineUnsafeCo

withNorm :: forall f r a.
     Record f r
  -> (    Rep (AfterUnI f) (Record I r)
       -> (forall f'. Rep (AfterUnI f') (Record I r) -> Record f' r)
       -> a
     )
  -> a
withNorm r k =
    with' (co r) $ \rep fromRep ->
      k rep (co' . fromRep)
  where
    co  :: forall f'. Record f' r -> Record (AfterUnI f' :.: I) r
    co' :: forall f'. Record (AfterUnI f' :.: I) r -> Record f' r

    co  = noInlineUnsafeCo
    co' = noInlineUnsafeCo

{-------------------------------------------------------------------------------
  Conversions

  The downside of these functions over the lens-like API above is that the
  @to@ functions need the @KnownFields@ constraint.
-------------------------------------------------------------------------------}

fromRecord' :: Record (f :.: g) r -> Rep f (Record g r)
fromRecord' r = with' r $ \rep _fromRep -> rep

fromRecord :: Record f r -> Rep I (Record f r)
fromRecord r = with r $ \rep _fromRep -> rep

fromRecordNorm :: Record f r-> Rep (AfterUnI f) (Record I r)
fromRecordNorm r = withNorm r $ \rep _fromRep -> rep

toRecord' :: forall r f g.
     KnownFields r
  => Rep f (Record g r) -> Record (f :.: g) r
toRecord' (Rep values) = Record.unsafeFromCanonical $
    Canon.fromVector (fieldNames (Proxy @r)) (Strict.fromLazy (co values))
  where
    co :: Lazy.Vector (f Any) -> Lazy.Vector ((f :.: g) Any)
    co = noInlineUnsafeCo

toRecord :: KnownFields r => Rep I (Record f r) -> Record f r
toRecord = co . toRecord'
  where
    co :: Record (I :.: f') r -> Record f' r
    co = noInlineUnsafeCo

toRecordNorm :: KnownFields r => Rep (AfterUnI f) (Record I r) -> Record f r
toRecordNorm = co . toRecord'
  where
    co :: Record (AfterUnI f' :.: I) r -> Record f' r
    co = noInlineUnsafeCo

