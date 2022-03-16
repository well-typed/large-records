{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Record.Anonymous.Internal.StrictVector (
    Vector  -- opaque
  , MVector -- opaque
    -- * Conversion
  , toLazy
  , fromLazy
  , fromList
    -- * Hybrid functions
  , zipWithLazy
  ) where

import Data.Function (on)

import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as G
import qualified Data.Vector.Generic.Mutable      as GM
import qualified Data.Vector.Generic.Mutable.Base as GMB
import Control.Monad

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Strict vector
newtype Vector a = WrapLazy { unwrapLazy :: V.Vector a }
  deriving newtype (Show, Eq, Foldable, Semigroup, Monoid)

-- | Mutable strict vector
newtype MVector s a = WrapLazyM { unwrapLazyM :: V.MVector s a }

{-------------------------------------------------------------------------------
  MVector and Vector instances

  These instances make it possible to use the "Data.Vector.Generic" API.
-------------------------------------------------------------------------------}

type instance G.Mutable Vector = MVector

instance GMB.MVector MVector a where
  -- Forwarding definitions

  basicLength          = GMB.basicLength . unwrapLazyM
  basicUnsafeSlice i j = WrapLazyM . GMB.basicUnsafeSlice i j . unwrapLazyM
  basicOverlaps        = GMB.basicOverlaps `on` unwrapLazyM
  basicUnsafeNew n     = WrapLazyM <$> GMB.basicUnsafeNew n
  basicInitialize      = GMB.basicInitialize . unwrapLazyM
  basicUnsafeRead v i  = GMB.basicUnsafeRead (unwrapLazyM v) i
  basicClear           = GMB.basicClear . unwrapLazyM
  basicUnsafeCopy      = GMB.basicUnsafeCopy `on` unwrapLazyM
  basicUnsafeMove      = GMB.basicUnsafeMove `on` unwrapLazyM
  basicUnsafeGrow v i  = WrapLazyM <$> GMB.basicUnsafeGrow (unwrapLazyM v) i

  -- Definitions that actually enforce strictness

  basicUnsafeWrite v i !a = GMB.basicUnsafeWrite (unwrapLazyM v) i a
  basicSet         v   !a = GMB.basicSet (unwrapLazyM v) a

instance G.Vector Vector a where
  -- Forwarding definitions

  basicUnsafeFreeze     = fmap WrapLazy  . G.basicUnsafeFreeze . unwrapLazyM
  basicUnsafeThaw       = fmap WrapLazyM . G.basicUnsafeThaw   . unwrapLazy
  basicLength           = G.basicLength . unwrapLazy
  basicUnsafeSlice i j  = WrapLazy . G.basicUnsafeSlice i j . unwrapLazy
  basicUnsafeIndexM v i = G.basicUnsafeIndexM (unwrapLazy v) i
  basicUnsafeCopy v v'  = G.basicUnsafeCopy (unwrapLazyM v) (unwrapLazy v')

  -- Definitions that actually enforce strictness

  elemseq _v = seq

{-------------------------------------------------------------------------------
  Functor and Traversable instances

  We have to be careful here to preserve the strictness property.
  ('Foldable' can be derived.)
-------------------------------------------------------------------------------}

instance Functor Vector where
  fmap f v = WrapLazy $ G.create $ do
      v' <- GM.new (G.length v)
      G.iforM_ (unwrapLazy v) $ \i a -> do
        let !b = f a
        GM.unsafeWrite v' i b
      return v'

instance Traversable Vector where
  -- This is identical to the 'Traversable' instance for regular 'Vector',
  -- apart from the call to 'forceElems'. Since 'forceElems' is lazy, this does
  -- not result in any additional traversals.
  traverse f v =
      G.fromListN (G.length v) . forceListElems <$>
        traverse f (G.toList (unwrapLazy v))

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toLazy :: Vector a -> V.Vector a
toLazy = unwrapLazy

fromLazy :: V.Vector a -> Vector a
fromLazy v = WrapLazy $ G.create $ do
    v' <- GM.new (G.length v)
    G.iforM_ v $ \i !a -> GM.unsafeWrite v' i a
    return v'

fromList :: [a] -> Vector a
fromList = WrapLazy . V.fromList . forceListElems

{-------------------------------------------------------------------------------
  Hybrid functions
-------------------------------------------------------------------------------}

-- | Zip strict and lazy vector
--
-- Precondition: both vectors must have the same length
zipWithLazy :: (a -> b -> c) -> Vector a -> V.Vector b -> Vector c
zipWithLazy f va vb = WrapLazy $ G.create $ do
    unless (G.length va == G.length vb) $
      error "zipWithLazy: precondition violation"
    vc <- GM.new (G.length va)
    G.iforM_ va $ \i a -> do
      let b  = G.unsafeIndex vb i
          !c = f a b
      GM.unsafeWrite vc i c
    return vc

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Tie evaluation of the spine of the list to evaluation of its elements
--
-- 'forceElems' ensures that as the list is being traversed (to create the
-- new vector), the elements of the list are forced to WHNF one by one.
forceListElems :: [a] -> [a]
forceListElems []      = []
forceListElems (!a:as) = a:forceListElems as
