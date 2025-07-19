{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.Record.Anon.Internal.Util.StrictArray (
    StrictArray -- opaque
    -- * Array index
  , ArrayIndex(..)
  , ZeroBasedIndex(..)
  , ReverseIndex(..)
    -- * Reads
  , (!)
    -- * Conversion
  , fromList
  , fromListN
  , fromLazy
  , toLazy
    -- * Non-monadic combinators
  , (//)
  , update
  , backpermute
  , zipWith
    -- * Monadic combinators
  , mapM
  , zipWithM
  ) where

import Prelude hiding (mapM, zipWith)

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Primitive.SmallArray hiding (writeSmallArray, indexSmallArray)

import qualified Control.Monad             as Monad
import qualified Data.Foldable             as Foldable
import qualified Data.Primitive.SmallArray as SmallArray

#ifdef DEBUG
import GHC.Stack
#endif

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Strict vector
--
-- Implemented as a wrapper around a 'SmallArray'.
--
-- NOTE: The operations on 'Vector' do bounds checking only if the @debug@ flag
-- is enabled.
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
newtype StrictArray i a = WrapLazy { unwrapLazy :: SmallArray a }
  deriving newtype (Show, Eq, Foldable, Semigroup, Monoid)

{-------------------------------------------------------------------------------
  Array index
-------------------------------------------------------------------------------}

class ArrayIndex i where
  -- | Compute 0-based index from @i@, given the size of the array
  arrayIndex :: Int -> i -> Int

newtype ZeroBasedIndex = ZeroBasedIndex { getZeroBasedIndex :: Int }

instance ArrayIndex ZeroBasedIndex where
  arrayIndex _size = getZeroBasedIndex

-- | Index from the /end/ of the array
--
-- @ReverseIndex 0@ points to the final element.
newtype ReverseIndex = ReverseIndex { getReverseIndex :: Int }

instance ArrayIndex ReverseIndex where
  arrayIndex size i = size - 1 - getReverseIndex i

{-------------------------------------------------------------------------------
  Reads
-------------------------------------------------------------------------------}

(!) :: ArrayIndex i => StrictArray i a -> i -> a
(!) = indexSmallArray . unwrapLazy

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

fromList :: [a] -> StrictArray i a
fromList as = fromListN (length as) as

fromListN :: Int -> [a] -> StrictArray i a
fromListN n as = WrapLazy $ runSmallArray $ do
    r <- newSmallArray n undefined
    forM_ (zip [0..] as) $ \(i, !a) ->
      writeSmallArray r (ZeroBasedIndex i) a
    return r

fromLazy :: forall i a. SmallArray a -> StrictArray i a
fromLazy v = go 0
  where
    go :: Int -> StrictArray i a
    go i
      | i < sizeofSmallArray v
      = let !_a = indexSmallArray v (ZeroBasedIndex i)
        in go (succ i)

      | otherwise
      = WrapLazy v

toLazy :: StrictArray i a -> SmallArray a
toLazy = unwrapLazy

{-------------------------------------------------------------------------------
  Non-monadic combinators
-------------------------------------------------------------------------------}

instance Functor (StrictArray i) where
  fmap f (WrapLazy as) = WrapLazy $ runSmallArray $ do
      r <- newSmallArray newSize undefined
      forArrayM_ as $ \i a -> writeSmallArray r i $! f a
      return r
    where
      newSize :: Int
      newSize = sizeofSmallArray as

(//) :: ArrayIndex i => StrictArray i a -> [(i, a)] -> StrictArray i a
(//) (WrapLazy as) as' = WrapLazy $ runSmallArray $ do
    r <- thawSmallArray as 0 newSize
    forM_ as' $ \(i, !a) -> writeSmallArray r i a
    return r
  where
    newSize :: Int
    newSize = sizeofSmallArray as

update ::
     ArrayIndex i
  => StrictArray i a  -- ^ Array to update
  -> [(i, a)]         -- ^ Indices into the original array and their new value
                      --   (the order of this list is irrelevant)
  -> StrictArray i a
update (WrapLazy as) as' = WrapLazy $ runSmallArray $ do
    r <- thawSmallArray as 0 newSize
    forM_ as' $ \(j, !a) -> writeSmallArray r j a
    return r
  where
    newSize :: Int
    newSize = sizeofSmallArray as

backpermute ::
     ArrayIndex i
  => StrictArray i a   -- ^ Array to take values from
  -> [i]               -- ^ List of indices into the source array,
                       --   in the order they must appear in the result array
  -> StrictArray i a
backpermute (WrapLazy as) is = WrapLazy $ runSmallArray $ do
    r <- newSmallArray newSize undefined
    forM_ (zip [0..] is) $ \(i, j) ->
      writeSmallArray r (ZeroBasedIndex i) $! indexSmallArray as j
    return r
  where
    newSize :: Int
    newSize = length is

zipWith ::
     (a -> b -> c)
  -> StrictArray i a -> StrictArray i b -> StrictArray i c
zipWith f (WrapLazy as) (WrapLazy bs) = WrapLazy $ runSmallArray $ do
    r <- newSmallArray newSize undefined
    forM_ [0 .. newSize - 1] $ \i -> do
      let !c = f (indexSmallArray as (ZeroBasedIndex i))
                 (indexSmallArray bs (ZeroBasedIndex i))
      writeSmallArray r (ZeroBasedIndex i) c
    return r
  where
    newSize :: Int
    newSize = min (sizeofSmallArray as) (sizeofSmallArray bs)

{-------------------------------------------------------------------------------
  Applicative combinators

  NOTE: The monadic combinators here do two traversals, first collecting all
  elements of the vector in memory, and then constructing the new vector. The
  alternative is to use 'traverseSmallArrayP', but it is only sound with
  certain monads. Since this restriction would leak out to users of the library
  (through the monadic combinators on 'Record'), we prefer to avoid it.
-------------------------------------------------------------------------------}

mapM :: forall m i a b.
     Applicative m
  => (a -> m b) -> StrictArray i a -> m (StrictArray i b)
mapM f (WrapLazy as) =
    fromListN newSize <$>
      traverse f (Foldable.toList as)
  where
    newSize :: Int
    newSize = sizeofSmallArray as

zipWithM ::
     Applicative m
  => (a -> b -> m c)
  -> StrictArray i a -> StrictArray i b -> m (StrictArray i c)
zipWithM f (WrapLazy as) (WrapLazy bs) = do
    fromListN newSize <$>
      Monad.zipWithM f (Foldable.toList as) (Foldable.toList bs)
  where
    newSize :: Int
    newSize = min (sizeofSmallArray as) (sizeofSmallArray bs)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

forArrayM_ :: forall m a.
     Monad m
  => SmallArray a -> (ZeroBasedIndex -> a -> m ()) -> m ()
forArrayM_ arr f = go 0
  where
    go :: Int -> m ()
    go i
      | i < sizeofSmallArray arr = do
          f (ZeroBasedIndex i) (indexSmallArray arr (ZeroBasedIndex i))
          go (succ i)
      | otherwise =
          return ()

{-------------------------------------------------------------------------------
  Interpreting 'ArrayIndex'

  Bounds checking is only enabled when built with the @debug@ flag set.
-------------------------------------------------------------------------------}

indexSmallArray :: ArrayIndex i => SmallArray r -> i -> r
indexSmallArray arr i = boundsCheck arr i' $
    SmallArray.indexSmallArray arr i'
  where
    i' :: Int
    i' = arrayIndex (sizeofSmallArray arr) i

writeSmallArray :: ArrayIndex i => SmallMutableArray s a -> i -> a -> ST s ()
writeSmallArray arr i a = do
    sz <- getSizeofSmallMutableArray arr
    let i' = arrayIndex sz i
    boundsCheckM arr i' $
      SmallArray.writeSmallArray arr i' a

#ifdef DEBUG
boundsCheck :: HasCallStack => SmallArray a -> Int -> r -> r
boundsCheck arr i k =
    if 0 <= i && i < sizeofSmallArray arr
      then k
      else error $ concat [
               "StrictArray: index " ++ show i ++ " out of bounds"
             , " (array size: " ++ show (sizeofSmallArray arr) ++ ")"
             ]
#else
boundsCheck :: SmallArray a -> Int -> r -> r
boundsCheck _arr _i k = k
#endif

#ifdef DEBUG
boundsCheckM :: HasCallStack => SmallMutableArray s a -> Int -> ST s r -> ST s r
boundsCheckM arr i k = do
    sz <- getSizeofSmallMutableArray arr
    if 0 <= i && i < sz
      then k
      else error $ concat [
               "StrictArray: index " ++ show i ++ " out of bounds"
             , " (array size: " ++ show sz ++ ")"
             ]
#else
boundsCheckM :: SmallMutableArray s a -> Int -> ST s r -> ST s r
boundsCheckM _arr _i k = k
#endif

{-------------------------------------------------------------------------------
  Auxiliary: support primitive < 0.9
-------------------------------------------------------------------------------}

#if !MIN_VERSION_primitive(0,9,0)
getSizeofSmallMutableArray :: SmallMutableArray s a -> ST s Int
getSizeofSmallMutableArray = return . sizeofSmallMutableArray
#endif