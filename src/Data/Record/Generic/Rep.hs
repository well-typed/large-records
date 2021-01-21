{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Operations on the generic representation
--
-- We also re-export some non-derive functions to clarify where they belong
-- in this list.
--
-- This module is intended for qualified import.
--
-- > import qualified Data.Record.Generic.Rep as Rep
module Data.Record.Generic.Rep (
    -- | Mapping
    map
  , mapM
  , cmap
  , cmapM
    -- | Zipping
  , zip
  , zipWith
  , zipWithM
  , czipWith
  , czipWithM
    -- | Folding
  , collapse
    -- | Traversing
  , sequenceA
    -- | Generation
  , pure
  , cpure
    -- | Conversion
  , unsafeFromListK
  ) where

import Prelude hiding (
    map
  , mapM
  , pure
  , sequenceA
  , zip
  , zipWith
  )
import qualified Prelude

import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Product

import qualified Data.Vector as V

import Data.Record.Generic

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Rep f a -> Rep g a
map f (Rep v) = Rep $ f <$> v

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Rep f a -> m (Rep g a)
mapM f (Rep v) = Rep <$> traverse f v

cmap ::
     (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x)
  -> Rep f a -> Rep g a
cmap p f = runIdentity . cmapM p (Identity . f)

cmapM ::
     (Generic a, Applicative r, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> r (g x))
  -> Rep f a -> r (Rep g a)
cmapM p f a = czipWithM p (\x _ -> f x) a (pure (K ()))

{-------------------------------------------------------------------------------
  Zipping
-------------------------------------------------------------------------------}

zip :: Rep f a -> Rep g a -> Rep (Product f g) a
zip = zipWith Pair

zipWith ::
     (forall x. f x -> g x -> h x)
  -> Rep f a -> Rep g a -> Rep h a
zipWith f (Rep a) (Rep b) = Rep $ V.zipWith f a b

zipWithM ::
     Applicative m
  => (forall x. f x -> g x -> m (h x))
  -> Rep f a -> Rep g a -> m (Rep h a)
zipWithM f (Rep a) (Rep b) = Rep <$>
    -- The 'Applicative' instance on 'Vector' behaves like @[]@, not @ZipList@
    -- 'V.zipWithM' requires 'Monad' rather than 'Applicative'
    Prelude.sequenceA (V.zipWith f a b)

czipWith ::
     (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> h x)
  -> Rep f a -> Rep g a -> Rep h a
czipWith p f a b = runIdentity (czipWithM p (\x y -> Identity (f x y)) a b)

czipWithM ::
     forall m f g h c a. (Generic a, Applicative m, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x -> g x -> m (h x))
  -> Rep f a -> Rep g a -> m (Rep h a)
czipWithM p f a b =
    sequenceA (zipWith apFn (zipWith apFn (pure f') (dict p)) (zip a b))
  where
    f' :: (Dict c -.-> Product f g -.-> m :.: h) x
    f' = Fn $ \Dict -> Fn $ \(Pair x y) -> Comp (f x y)

{-------------------------------------------------------------------------------
  Folding
-------------------------------------------------------------------------------}

collapse :: Rep (K a) b -> [a]
collapse = getConst . mapM (\(K a) -> Const [a])

{-------------------------------------------------------------------------------
  Traversing
-------------------------------------------------------------------------------}

sequenceA :: Applicative m => Rep (m :.: f) a -> m (Rep f a)
sequenceA (Rep v) = Rep <$> Prelude.sequenceA (fmap unComp v)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

pure :: forall f a. Generic a => (forall x. f x) -> Rep f a
pure = Rep . V.replicate (recordSize (Proxy @a))

cpure ::
     forall c f a. (Generic a, Constraints a c)
  => Proxy c
  -> (forall x. c x => f x)
  -> Rep f a
cpure p f = zipWith apFn (pure f') (dict p)
  where
    f' :: forall x. (Dict c -.-> f) x
    f' = Fn $ \Dict -> f

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Convert list to 'Rep'
--
-- Does not check that the length has the right number of elements.
unsafeFromListK :: [b] -> Rep (K b) a
unsafeFromListK = Rep . V.fromList . Prelude.map K
