{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Definition of 'Rep' and functions that do not depend on ".Generic"
--
-- Defined as a separate module to avoid circular module dependencies.
module Data.Record.Generic.Rep.Internal (
    Rep(..)
    -- | "Functor"
  , map
  , mapM
    -- | Zipping
  , zip
  , zipWith
  , zipWithM
    -- | "Foldable"
  , collapse
    -- | "Traversable"
  , sequenceA
    -- | "Applicable"
  , ap
    -- | Conversion
  , unsafeFromListK
  ) where

import Prelude hiding (
    map
  , mapM
  , sequenceA
  , zip
  , zipWith
  )
import qualified Prelude

import Data.Functor.Const
import Data.Functor.Product
import Data.SOP.BasicFunctors
import Data.SOP.Classes (type (-.->)(..))
import Data.Vector (Vector)
import GHC.Exts (Any)

import qualified Data.Vector as V

-- | Representation of some record @a@
--
-- The @f@ parameter describes which functor has been applied to all fields of
-- the record; in other words @Rep I@ is isomorphic to the record itself.
newtype Rep f a = Rep (Vector (f Any))

type role Rep representational nominal

{-------------------------------------------------------------------------------
  "Functor"
-------------------------------------------------------------------------------}

map :: (forall x. f x -> g x) -> Rep f a -> Rep g a
map f (Rep v) = Rep $ f <$> v

mapM ::
     Applicative m
  => (forall x. f x -> m (g x))
  -> Rep f a -> m (Rep g a)
mapM f (Rep v) = Rep <$> traverse f v

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

{-------------------------------------------------------------------------------
  "Foldable"
-------------------------------------------------------------------------------}

collapse :: Rep (K a) b -> [a]
collapse = getConst . mapM (\(K a) -> Const [a])

{-------------------------------------------------------------------------------
  "Traversable"
-------------------------------------------------------------------------------}

sequenceA :: Applicative m => Rep (m :.: f) a -> m (Rep f a)
sequenceA (Rep v) = Rep <$> Prelude.sequenceA (fmap unComp v)

{-------------------------------------------------------------------------------
  "Applicable"
-------------------------------------------------------------------------------}

ap :: Rep (f -.-> g) a -> Rep f a -> Rep g a
ap = zipWith apFn

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Convert list to 'Rep'
--
-- Does not check that the length has the right number of elements.
unsafeFromListK :: [b] -> Rep (K b) a
unsafeFromListK = Rep . V.fromList . Prelude.map K

{-------------------------------------------------------------------------------
  Some specialised instances for 'Rep
-------------------------------------------------------------------------------}

instance Show x => Show (Rep (K x) a) where
  show (Rep v) =
      show $ Prelude.map unK (V.toList v)

instance Eq x => Eq (Rep (K x) a) where
  Rep v == Rep v' =
         Prelude.map unK (V.toList v)
      == Prelude.map unK (V.toList v')

