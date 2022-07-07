{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- | Strict hash table designed for small hash tables
--
-- Currently this is is just a wrapper around a 'Map'. We do not use 'HashMap',
-- since for small hash tables the overhead from having to copy all the small
-- arrays defeats the purpose of having a 'Diff' in the first place.
--
-- Having this as a separate abstraction also allows us to easily change the
-- representation of the 'HashMap' without affecting the rest of the code.
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon.Internal.Util.SmallHashMap (SmallHashMap)
-- > import qualified Data.Record.Anon.Internal.Util.SmallHashMap as HashMap
module Data.Record.Anon.Internal.Util.SmallHashMap (
    SmallHashMap
    -- * Standard operations
  , null
  , empty
  , lookup
  , member
  , insert
  , toList
  , alter
    -- * Non-standard operations
  , alterExisting
  ) where

import Prelude hiding (lookup, null)

import Control.Monad.State
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Hashable (Hashable(hash))
import Data.Map.Strict (Map)
import Data.Tuple (swap)

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
  Wrapper to compare keys based on their hash first
-------------------------------------------------------------------------------}

newtype Hashed k = Hashed k
  deriving (Show)

instance Hashable k => Eq (Hashed k) where
  Hashed a == Hashed b = and [
      hash a == hash b
    ,      a ==      b
    ]

instance (Hashable k, Ord k) => Ord (Hashed k) where
  compare (Hashed a) (Hashed b) = mconcat [
        compare (hash a) (hash b)
      , compare       a        b
      ]

{-------------------------------------------------------------------------------
  Definition of the HashMap proper
-------------------------------------------------------------------------------}

newtype SmallHashMap k a = Wrap { unwrap :: Map (Hashed k) a }
  deriving (Show)

-- | Cannot derive 'Functor' because the 'Functor' instance for 'Map' is wrong
-- (not strict)
instance Functor (SmallHashMap k) where
  fmap f = Wrap . Map.map f . unwrap

{-------------------------------------------------------------------------------
  Standard operations
-------------------------------------------------------------------------------}

null :: forall k a. SmallHashMap k a -> Bool
null = coerce $ Map.null @(Hashed k) @a

empty :: forall k a. SmallHashMap k a
empty = coerce $ Map.empty @(Hashed k) @a

lookup :: forall k a. (Hashable k, Ord k) => k -> SmallHashMap k a -> Maybe a
lookup = coerce $ Map.lookup @(Hashed k) @a

member :: forall k a. (Hashable k, Ord k) => k -> SmallHashMap k a -> Bool
member = coerce $ Map.member @(Hashed k) @a

insert :: forall k a.
     (Hashable k, Ord k)
  => k -> a -> SmallHashMap k a -> SmallHashMap k a
insert = coerce $ Map.insert @(Hashed k) @a

toList :: forall k a. SmallHashMap k a -> [(k, a)]
toList = coerce $ Map.toList @(Hashed k) @a

alter :: forall k a.
     (Hashable k, Ord k)
  => (Maybe a -> Maybe a) -> k -> SmallHashMap k a -> SmallHashMap k a
alter = coerce $ Map.alter @(Hashed k) @a

{-------------------------------------------------------------------------------
  Non-standard operations
-------------------------------------------------------------------------------}

-- | Alter an existing key
--
-- Returns 'Nothing' if the key does not exist.
--
-- @O(1)@.
alterExisting :: forall k a b.
     (Hashable k, Ord k)
  => k -> (a -> (b, Maybe a)) -> SmallHashMap k a -> Maybe (b, SmallHashMap k a)
alterExisting k f m
  | null m    = Nothing
  | otherwise =
      fmap (second Wrap . swap)
    . distrib
    . flip runState Nothing
    . Map.alterF f' (Hashed k)
    . unwrap
    $ m
  where
    f' :: Maybe a -> State (Maybe b) (Maybe a)
    f' Nothing  = state $ \_ -> (Nothing, Nothing)
    f' (Just a) = state $ \_ -> swap $ first Just (f a)

    distrib :: (x, Maybe y) -> Maybe (x, y)
    distrib (x, my) = (x,) <$> my
