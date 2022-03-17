{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase #-}

-- | Utilities for working with hashmaps
--
-- Intended for qualified import:
--
-- > import qualified Data.Record.Anonymous.Internal.Util.HashMap as HashMap
module Data.Record.Anonymous.Internal.Util.HashMap (
    -- * Miscellaneous
    alterExisting
    -- * Merging
  , Merged
  , merge
  , merged
  ) where

import Prelude hiding (abs)

import Control.Monad.State
import Data.Bifunctor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Tuple (swap)

import qualified Data.HashMap.Strict as HashMap

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Alter an existing key
--
-- Returns 'Nothing' if the key does not exist.
--
-- @O(1)@.
alterExisting :: forall k a b.
     (Hashable k, Eq k)
  => k -> (a -> (b, Maybe a)) -> HashMap k a -> Maybe (b, HashMap k a)
alterExisting k f =
    fmap swap . distrib . flip runState Nothing . HashMap.alterF f' k
  where
    f' :: Maybe a -> State (Maybe b) (Maybe a)
    f' Nothing  = state $ \_ -> (Nothing, Nothing)
    f' (Just a) = state $ \_ -> swap $ first Just (f a)

    distrib :: (x, Maybe y) -> Maybe (x, y)
    distrib (x, my) = (x,) <$> my

{-------------------------------------------------------------------------------
  Merging
-------------------------------------------------------------------------------}

data Merged a b =
    InLeft a
  | InRight b
  | InBoth a b

-- | Eliminator for 'Merged'
merged :: (Either a b -> c) -> ((a, b) -> c) -> Merged a b -> c
merged inOne inBoth = \case
    InLeft  a   -> inOne  (Left a)
    InRight   b -> inOne  (Right b)
    InBoth  a b -> inBoth (a, b)

-- | Merge two 'HashMap's
merge ::
     (Hashable k, Eq k)
  => HashMap k a -> HashMap k b -> HashMap k (Merged a b)
merge as bs = HashMap.unionWith aux (InLeft <$> as) (InRight <$> bs)
  where
    aux :: Merged a b -> Merged a b -> Merged a b
    aux (InLeft a) (InRight b) = InBoth a b
    aux _ _ = error "merge.aux: impossible"
