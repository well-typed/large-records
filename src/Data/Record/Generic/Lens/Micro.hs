{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Generic.Lens.Micro (
    -- * Lenses into a large record
    MicroLens(..)
  , glenses
    -- * Low level: lenses into 'Rep'
  , RepLens(..)
  , repLenses
    -- * Specific lenses
  , genericLens
  , normalForm1Lens
  , interpretedLens
  ) where

import Lens.Micro (Lens')

import Data.Record.Generic
import Data.Record.Generic.Transform

import qualified Data.Record.Generic.Rep as Rep

{-------------------------------------------------------------------------------
  Lenses into a large record
-------------------------------------------------------------------------------}

data MicroLens a b where
  MicroLens :: Lens' a b -> MicroLens a b

-- | Construct lenses for each field in the record
--
-- TODO: This is currently of limited use since we cannot pattern match on the
-- resulting 'Rep' in any meaningful way. It is possible to go through the
-- SOP adapter, but if we do, we incur quadratic cost again.
--
-- The lower-level function 'RepLens' is probably more useful.
glenses :: forall a. Generic a => Rep (MicroLens a) a
glenses =
    Rep.map (\(RepLens l) -> MicroLens $ \f -> aux l f) repLenses
  where
    aux ::
         Functor f
      => ((I x -> f (I x)) -> Rep I a -> f (Rep I a))
      -> (x -> f x)
      -> a -> f a
    aux l f a = to <$> l (\(I x) -> I <$> f x) (from a)

{-------------------------------------------------------------------------------
  Low level: lenses into 'Rep'
-------------------------------------------------------------------------------}

data RepLens f a x where
  RepLens :: Lens' (Rep f a) (f x) -> RepLens f a x

repLenses :: Generic a => Rep (RepLens f a) a
repLenses = Rep.map aux Rep.allIndices
  where
    aux :: Rep.Index a x -> RepLens f a x
    aux ix = RepLens $ Rep.updateAtIndex ix

{-------------------------------------------------------------------------------
  Specific lenses
-------------------------------------------------------------------------------}

genericLens :: Generic a => Lens' a (Rep I a)
genericLens f a = to <$> f (from a)

normalForm1Lens ::
     HasNormalForm (d f) (x f) (x Uninterpreted)
  => Proxy d
  -> Lens' (Rep I (x f)) (Rep (Interpret (d f)) (x Uninterpreted))
normalForm1Lens p f a = denormalize1 p <$> f (normalize1 p a)

interpretedLens :: Lens' (Interpret d x) (Interpreted d x)
interpretedLens f (Interpret x) = Interpret <$> f x
