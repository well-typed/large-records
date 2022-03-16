{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Simple (non-constrained) 'Record' combinators
--
-- These names (intentionally) overlap with 'Prelude' names.
module Data.Record.Anonymous.Internal.Combinators.Simple (
    -- * "Functor"
    map
  , mapM
    -- * Zipping
  , zip
  , zipWith
  , zipWithM
    -- * "Foldable"
  , collapse
    -- * "Traversable"
  , sequenceA
    -- * "Applicable"
  , pure
  , ap
  ) where

import Prelude hiding (map, mapM, zip, zipWith, sequenceA, pure)
import qualified Prelude

import Data.Functor.Product
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Classes (type (-.->))

import Data.Record.Anonymous.Internal.Canonical (Canonical)
import Data.Record.Anonymous.Internal.Record (Record(..))
import Data.Record.Anonymous.Internal.Row

import qualified Data.Record.Anonymous.Internal.Canonical as Canon
import qualified Data.Record.Anonymous.Internal.Record    as Record

{-------------------------------------------------------------------------------
  Internal auxiliary (convenient shorthand)
-------------------------------------------------------------------------------}

c :: Record f r -> Canonical f
c = Record.canonicalize

fromC :: Canonical f -> Record f r
fromC = Record.unsafeFromCanonical

{-------------------------------------------------------------------------------
  Combinators proper
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Record f r -> Record g r
map f (c -> r) = fromC $ Canon.map f r

mapM ::
     Monad m
  => (forall x. f x -> m (g x))
  -> Record f r -> m (Record g r)
mapM f (c -> r) = fromC <$> Canon.mapM f r

zip :: Record f r -> Record g r -> Record (Product f g) r
zip = zipWith Pair

zipWith ::
     (forall x. f x -> g x -> h x)
  -> Record f r -> Record g r -> Record h r
zipWith f (c -> r) (c -> r') = fromC $ Canon.zipWith f r r'

zipWithM ::
     Monad m
  => (forall x. f x -> g x -> m (h x))
  -> Record f r -> Record g r -> m (Record h r)
zipWithM f (c -> r) (c -> r') = fromC <$> Canon.zipWithM f r r'

collapse :: Record (K a) r -> [a]
collapse (c -> r) = Canon.collapse r

sequenceA :: Monad m => Record (m :.: f) r -> m (Record f r)
sequenceA (c -> r) = fromC <$> Canon.sequenceA r

pure :: forall f r. KnownFields r => (forall x. f x) -> Record f r
pure f = fromC $ Canon.fromList $ Prelude.map (const f) (fieldNames (Proxy @r))

ap :: Record (f -.-> g) r -> Record f r -> Record g r
ap (c -> r) (c -> r') = fromC $ Canon.ap r r'
