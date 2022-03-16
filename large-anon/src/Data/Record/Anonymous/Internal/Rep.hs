{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

-- | Conversion between 'Record' and @large-generics@ 'Rep'
--
-- Intended for qualified import.
module Data.Record.Anonymous.Internal.Rep (
    -- * Conversions
    fromRecord
  , fromRecordNorm
  , fromRecord'
  , toRecord
  , toRecordNorm
  , toRecord'
  ) where

import Data.Record.Generic.Rep.Internal (Rep(..), noInlineUnsafeCo)
import Data.SOP.BasicFunctors
import GHC.Exts (Any)

import qualified Data.Vector as Lazy

import Data.Record.Anonymous.Internal.AfterUnI
import Data.Record.Anonymous.Internal.Record (Record)

import qualified Data.Record.Anonymous.Internal.Canonical as Canon
import qualified Data.Record.Anonymous.Internal.Record    as Record

{-------------------------------------------------------------------------------
  Conversions

  The downside of these functions over the lens-like API above is that the
  @to@ functions need the @KnownFields@ constraint.
-------------------------------------------------------------------------------}

fromRecord' :: Record (f :.: g) r -> Rep f (Record g r)
fromRecord' (Record.canonicalize -> r) =
    Rep $ co . Canon.toLazyVector $ r
  where
    co :: Lazy.Vector ((f' :.: g') Any) -> Lazy.Vector (f' Any)
    co = noInlineUnsafeCo

toRecord' :: Rep f (Record g r) -> Record (f :.: g) r
toRecord' (Rep r) =
    Record.unsafeFromCanonical $ Canon.fromLazyVector . co $ r
  where
    co :: Lazy.Vector (f Any) -> Lazy.Vector ((f :.: g) Any)
    co = noInlineUnsafeCo

fromRecord :: Record f r -> Rep I (Record f r)
fromRecord = fromRecord' . co
  where
    co :: Record f' r -> Record (I :.: f') r
    co = noInlineUnsafeCo

toRecord :: Rep I (Record f r) -> Record f r
toRecord = co . toRecord'
  where
    co :: Record (I :.: f') r -> Record f' r
    co = noInlineUnsafeCo

fromRecordNorm :: Record f r -> Rep (AfterUnI f) (Record I r)
fromRecordNorm = fromRecord' . co
  where
    co :: Record f' r -> Record (AfterUnI f' :.: I) r
    co = noInlineUnsafeCo

toRecordNorm :: Rep (AfterUnI f) (Record I r) -> Record f r
toRecordNorm = co . toRecord'
  where
    co :: Record (AfterUnI f' :.: I) r -> Record f' r
    co = noInlineUnsafeCo

