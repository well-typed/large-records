{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE TypeOperators       #-}

-- | Definition of 'Rep' and functions that do not depend on ".Generic"
--
-- Defined as a separate module to avoid circular module dependencies.
module Data.Record.Generic.Rep.Internal (
    Rep(..)
    -- * Basic functions
  , map'
  , sequenceA
    -- * Conversion
  , unsafeFromList
  , unsafeFromListAny
  , collapse
  , toListAny
    -- * Auxiliary
  , noInlineUnsafeCo
  ) where

import Prelude hiding (sequenceA)
import qualified Prelude

import Data.Coerce (coerce)
import Data.SOP.BasicFunctors
import Data.Vector (Vector)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

{-------------------------------------------------------------------------------
  Representation
-------------------------------------------------------------------------------}

-- | Representation of some record @a@
--
-- The @f@ parameter describes which functor has been applied to all fields of
-- the record; in other words @Rep I@ is isomorphic to the record itself.
newtype Rep f a = Rep (Vector (f Any))

type role Rep representational nominal

{-------------------------------------------------------------------------------
  Basic functions
-------------------------------------------------------------------------------}

-- | Strict map
--
-- @map' f x@ is strict in @x@: if @x@ is undefined, @map f x@ will also be
-- undefined, even if @f@ never needs any values from @x@.
map' :: (forall x. f x -> g x) -> Rep f a -> Rep g a
map' f (Rep v) = Rep $ f <$> v

sequenceA :: Applicative m => Rep (m :.: f) a -> m (Rep f a)
sequenceA (Rep v) = Rep <$> Prelude.sequenceA (fmap unComp v)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

collapse :: Rep (K a) b -> [a]
collapse (Rep v) = coerce (V.toList v)

-- | Convert 'Rep' to list
toListAny :: Rep f a -> [f Any]
toListAny (Rep v) = V.toList v

-- | Convert list to 'Rep'
--
-- Does not check that the list has the right number of elements.
unsafeFromList :: [b] -> Rep (K b) a
unsafeFromList = Rep . V.fromList . Prelude.map K

-- | Convert list to 'Rep'
--
-- Does not check that the list has the right number of elements, nor the
-- types of those elements.
unsafeFromListAny :: [f Any] -> Rep f a
unsafeFromListAny = Rep . V.fromList

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Avoid potential segfault with ghc < 9.0
--
-- See <https://gitlab.haskell.org/ghc/ghc/-/issues/16893>.
-- I haven't actually seen this fail in large-records, but we saw it fail in
-- the compact representation branch of sop-core, and what we do here is not
-- so different, so better to play it safe.
noInlineUnsafeCo :: forall a b. a -> b
{-# NOINLINE noInlineUnsafeCo #-}
noInlineUnsafeCo = unsafeCoerce

