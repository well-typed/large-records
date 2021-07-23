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
    -- * Basic functions
  , map'
  , sequenceA
    -- * Conversion
  , unsafeFromList
  , collapse
    -- * Utility (for use in @.Rep@)
  , compileToHere
  ) where

import Prelude hiding (sequenceA)
import qualified Prelude

import Data.Coerce (coerce)
import Data.SOP.BasicFunctors
import Data.Vector (Vector)
import GHC.Exts (Any)
import Language.Haskell.TH

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

-- | Convert list to 'Rep'
--
-- Does not check that the length has the right number of elements.
unsafeFromList :: [b] -> Rep (K b) a
unsafeFromList = Rep . V.fromList . Prelude.map K

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
  Internal utility
-------------------------------------------------------------------------------}

compileToHere :: Q [Dec]
compileToHere = return []