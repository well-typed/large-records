{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Record.Anon.Internal.Core.Util.StrictVector (
    Vector -- opaque
    -- * Reads
  , (!)
    -- * Conversion
  , fromList
  , fromListN
  , fromLazy
  , toLazy
    -- * Non-monadic combinators
  , (//)
  , permute
  , zipWith
    -- * Monadic combinators
  , mapM
  , zipWithM
  ) where

import Prelude hiding (mapM, zipWith)
import qualified Prelude

import Data.Primitive.SmallArray

import qualified Control.Monad as M
import qualified Data.Foldable as Foldable
import qualified Data.Vector   as V

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Strict vector
--
-- Implemented as a wrapper around a 'SmallArray'.
--
-- NOTE: None of the operations on 'Vector' do any bounds checking.
--
-- NOTE: 'Vector' is implemented as a newtype around 'SmallArray', which in turn
-- is defined as
--
-- > data SmallArray a = SmallArray (SmallArray# a)
--
-- Furthermore, 'Canonical' is a newtype around 'Vector', which is then used in
-- 'Record' as
--
-- > data Record (f :: k -> Type) (r :: Row k) = Record {
-- >       recordCanon :: {-# UNPACK #-} !(Canonical f)
-- >     , ..
-- >     }
--
-- This means that 'Record' will have /direct/ access (no pointers) to the
-- 'SmallArray#'.
newtype Vector a = WrapLazy { unwrapLazy :: SmallArray a }
  deriving newtype (Show, Eq, Foldable, Semigroup, Monoid)

{-------------------------------------------------------------------------------
  Reads
-------------------------------------------------------------------------------}

(!) :: Vector a -> Int -> a
(!) = indexSmallArray . unwrapLazy

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromList :: [a] -> Vector a
fromList as = fromListN (length as) as

fromListN :: Int -> [a] -> Vector a
fromListN n as = WrapLazy $
    createSmallArray n undefined $ \r ->
      M.forM_ (zip [0..] as) $ \(i, !a) ->
        writeSmallArray r i a

fromLazy :: V.Vector a -> Vector a
fromLazy v = fromListN (V.length v) (V.toList v)

toLazy :: Vector a -> V.Vector a
toLazy (WrapLazy arr) = V.fromListN (sizeofSmallArray arr) (Foldable.toList arr)

{-------------------------------------------------------------------------------
  Non-monadic combinators
-------------------------------------------------------------------------------}

instance Functor Vector where
  fmap f (WrapLazy as) = WrapLazy $
      createSmallArray newSize undefined $ \r ->
        M.forM_ [0 .. newSize - 1] $ \i -> do
          let !b = f (indexSmallArray as i)
          writeSmallArray r i b
    where
      newSize :: Int
      newSize = sizeofSmallArray as

(//) :: Vector a -> [(Int, a)] -> Vector a
(//) (WrapLazy as) as' = WrapLazy $ runSmallArray $ do
    r <- thawSmallArray as 0 newSize
    M.forM_ as' $ \(i, !a) -> writeSmallArray r i a
    return r
  where
    newSize :: Int
    newSize = sizeofSmallArray as

permute :: Vector a -> [Int] -> Vector a
permute (WrapLazy as) is = WrapLazy $
    createSmallArray newSize undefined $ \r ->
      M.forM_ (zip [0..] is) $ \(i, j) -> do
        let !a = indexSmallArray as j
        writeSmallArray r i a
  where
    newSize :: Int
    newSize = length is

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith f (WrapLazy as) (WrapLazy bs) = WrapLazy $
    createSmallArray newSize undefined $ \r ->
       M.forM_ [0 .. newSize - 1] $ \i -> do
         let !c = f (indexSmallArray as i) (indexSmallArray bs i)
         writeSmallArray r i c
  where
    newSize :: Int
    newSize = min (sizeofSmallArray as) (sizeofSmallArray bs)

{-------------------------------------------------------------------------------
  Monadic combinators

  NOTE: The monadic combinators here do two traversals, first collecting all
  elements of the vector in memory, and then constructing the new vector. The
  alternative is to use 'traverseSmallArrayP', but it is only sound with
  certain monads. Since this restriction would leak out to users of the library
  (through the monadic combinators on 'Record'), we prefer to avoid it.
-------------------------------------------------------------------------------}

mapM :: forall m a b. Monad m => (a -> m b) -> Vector a -> m (Vector b)
mapM f (WrapLazy as) = do
    bs <- Prelude.mapM f (Foldable.toList as)
    return $ fromListN newSize bs
  where
    newSize :: Int
    newSize = sizeofSmallArray as

zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM f (WrapLazy as) (WrapLazy bs) = do
    cs <- M.zipWithM f (Foldable.toList as) (Foldable.toList bs)
    return $ fromListN newSize cs
  where
    newSize :: Int
    newSize = min (sizeofSmallArray as) (sizeofSmallArray bs)
