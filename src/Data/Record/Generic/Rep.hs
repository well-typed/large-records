{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Operations on the generic representation
--
-- We also re-export some non-derive functions to clarify where they belong
-- in this list.
--
-- This module is intended for qualified import.
--
-- > import qualified Data.Record.Generic.Rep as Rep
--
-- TODO: Could we provide instances for the @generics-sop@ type classes?
-- Might lessen the pain of switching between the two or using both?
module Data.Record.Generic.Rep (
    Rep(..) -- TODO: Make opaque?
    -- * "Functor"
  , map
  , mapM
  , cmap
  , cmapM
    -- * Zipping
  , zip
  , zipWith
  , zipWithM
  , czipWith
  , czipWithM
    -- * "Foldable"
  , collapse
    -- * "Traversable"
  , sequenceA
    -- * "Applicable"
  , pure
  , cpure
  , ap
    -- * Array-like interface
  , Index -- opaque
  , indexToInt
  , getAtIndex
  , putAtIndex
  , updateAtIndex
  , allIndices
  , mapWithIndex
  ) where

import Prelude hiding (
    map
  , mapM
  , pure
  , sequenceA
  , zip
  , zipWith
  )

import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Product
import Data.SOP.Classes (fn_2)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

import Data.Record.Generic
import Data.Record.Generic.Rep.Internal

--
-- NOTE: In order to avoid circular definitions, this module is strictly defined
-- in order: every function only depends on the functions defined before it. To
-- enforce this, we make use of 'compileToHere' to force ghc to compile the
-- module to that point.
--

{-------------------------------------------------------------------------------
  Array-like interface
-------------------------------------------------------------------------------}

newtype Index a x = UnsafeIndex Int

indexToInt :: Index a x -> Int
indexToInt (UnsafeIndex ix) = ix

getAtIndex :: Index a x -> Rep f a -> f x
getAtIndex (UnsafeIndex ix) (Rep v) =
    unsafeCoerce $ V.unsafeIndex v ix

putAtIndex :: Index a x -> f x -> Rep f a -> Rep f a
putAtIndex (UnsafeIndex ix) x (Rep v) = Rep $
    V.unsafeUpd v [(ix, unsafeCoerce x)]

updateAtIndex ::
     Functor m
  => Index a x
  -> (f x -> m (f x))
  -> Rep f a -> m (Rep f a)
updateAtIndex ix f a = (\x -> putAtIndex ix x a) <$> f (getAtIndex ix a)

allIndices :: forall a. Generic a => Rep (Index a) a
allIndices = Rep $ V.generate (recordSize (metadata (Proxy @a))) UnsafeIndex

-- | Map with index
--
-- This is an important building block in this module.
-- Crucially, @mapWithIndex f a@ is lazy in @a@, reading elements from @a@
-- only if and when @f@ demands them.
mapWithIndex ::
     forall f g a. Generic a
  => (forall x. Index a x -> f x -> g x)
  -> Rep f a -> Rep g a
mapWithIndex f as = map' f' allIndices
  where
    f' :: Index a x -> g x
    f' ix = f ix (getAtIndex ix as)
compileToHere -- ===============================================================

{-------------------------------------------------------------------------------
  "Applicative"
-------------------------------------------------------------------------------}

pure :: forall f a. Generic a => (forall x. f x) -> Rep f a
pure = Rep . V.replicate (recordSize (metadata (Proxy @a)))

cpure ::
     (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x)
  -> Rep f a
cpure p f = map' (\Dict -> f) (dict p)

-- | Higher-order version of @<*>@
--
-- Lazy in the second argument.
ap :: forall f g a. Generic a => Rep (f -.-> g) a -> Rep f a -> Rep g a
ap fs as = mapWithIndex f' fs
  where
    f' :: Index a x -> (-.->) f g x -> g x
    f' ix f = f `apFn` getAtIndex ix as

compileToHere -- ===============================================================

{-------------------------------------------------------------------------------
  "Functor"
-------------------------------------------------------------------------------}

map :: Generic a => (forall x. f x -> g x) -> Rep f a -> Rep g a
map f = mapWithIndex (const f)

mapM ::
     (Applicative m, Generic a)
  => (forall x. f x -> m (g x))
  -> Rep f a -> m (Rep g a)
mapM f = sequenceA . mapWithIndex (const (Comp . f))

cmap ::
     (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> Rep f a -> Rep g a
cmap p f = ap $ cpure p (Fn f)

cmapM ::
     forall m f g c a. (Generic a, Applicative m, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> m (g x))
  -> Rep f a -> m (Rep g a)
cmapM p f = sequenceA . cmap p (Comp . f)

compileToHere -- ===============================================================

{-------------------------------------------------------------------------------
  Zipping
-------------------------------------------------------------------------------}

zipWithM ::
     forall m f g h a. (Generic a, Applicative m)
  => (forall x. f x -> g x -> m (h x))
  -> Rep f a -> Rep g a -> m (Rep h a)
zipWithM f a b = sequenceA $
    pure (fn_2 $ \x y -> Comp $ f x y) `ap` a `ap` b

zipWith ::
     Generic a
  => (forall x. f x -> g x -> h x)
  -> Rep f a -> Rep g a -> Rep h a
zipWith f a b = runIdentity $
    zipWithM (\x y -> Identity $ f x y) a b

zip :: Generic a => Rep f a -> Rep g a -> Rep (Product f g) a
zip = zipWith Pair

czipWithM ::
     forall m f g h c a. (Generic a, Applicative m, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Rep f a -> Rep g a -> m (Rep h a)
czipWithM p f a b = sequenceA $
    cpure p (fn_2 $ \x y -> Comp $ f x y) `ap` a `ap` b

czipWith ::
     (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Rep f a -> Rep g a -> Rep h a
czipWith p f a b = runIdentity $
    czipWithM p (\x y -> Identity (f x y)) a b

compileToHere -- ===============================================================
