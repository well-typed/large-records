{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Record.Anonymous.Internal.StrictVector (
    Vector  -- opaque
  , MVector -- opaque
    -- * Conversion
  , toLazy
  , fromLazy
  , fromList
    -- * Combinators
  , mapM
  ) where

import Prelude hiding (mapM)

import Data.Function (on)

import qualified Data.Vector                       as V
import qualified Data.Vector.Generic               as G
import qualified Data.Vector.Generic.Mutable       as GM
import qualified Data.Vector.Generic.Mutable.Base  as GMB
import qualified Data.Vector.Fusion.Bundle.Monadic as FBM
import qualified Data.Vector.Fusion.Bundle.Size    as FBS
import qualified Data.Vector.Fusion.Stream.Monadic as FSM

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

{-
-- We /can/ provide a 'Traversable' instance, but it incurs two traversals.
-- Use 'mapM' instead.
instance Traversable Vector where
  -- This is identical to the 'Traversable' instance for regular 'Vector',
  -- apart from the call to 'forceElems'. Since 'forceElems' is lazy, this does
  -- not result in any additional traversals.
  traverse f v =
      G.fromListN (G.length v) . forceListElems <$>
        traverse f (G.toList (unwrapLazy v))
-}

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
  Combinators
-------------------------------------------------------------------------------}

mapM :: forall m a b. Monad m => (a -> m b) -> Vector a -> m (Vector b)
mapM f (WrapLazy v) = WrapLazy <$>
    G.unstreamM (FBM.fromStream stream (FBS.Exact (G.length v)))
  where
    stream :: FSM.Stream m b
    stream = FSM.Stream go 0
      where
        go :: Int -> m (FSM.Step Int b)
        go i | i >= G.length v = return FSM.Done
             | otherwise       = do !b <- f (G.unsafeIndex v i)
                                    return $ FSM.Yield b (succ i)

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
