{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Record.Generic (
    -- * Types with a generic view
    Generic(..)
  , Rep(..)
  , Metadata(..)
    -- * Re-exports
  , module SOP
  , Proxy(..)
  ) where

import Data.Kind
import Data.Proxy
import Data.Vector (Vector)
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import qualified Data.Vector as V

-- To reduce overlap between the two libraries and improve interoperability,
-- we import as much from sop-core as possible.
import Data.SOP.BasicFunctors as SOP
import Data.SOP.Classes       as SOP (type (-.->)(..))
import Data.SOP.Dict          as SOP (Dict(..))

{-------------------------------------------------------------------------------
  Generic type class
-------------------------------------------------------------------------------}

class Generic a where
  -- | @Constraints a c@ means "all fields of @a@ satisfy @c@"
  type Constraints a :: (Type -> Constraint) -> Constraint

  -- | Type-level metadata
  type MetadataOf a :: [(Symbol, Type)]

  -- | Translate to generic representation
  from :: a -> Rep I a

  -- | Translate from generic representation
  to :: Rep I a -> a

  -- | Construct vector of dictionaries, one for each field of the record
  dict :: Constraints a c => Proxy c -> Rep (Dict c) a

  -- | Metadata
  metadata :: proxy a -> Metadata a

-- | Representation of some record @a@
--
-- The @f@ parameter describes which functor has been applied to all fields of
-- the record; in other words @Rep I@ is isomorphic to the record itself.
newtype Rep f a = Rep (Vector (f Any))

data Metadata a = Metadata {
      recordName        :: String
    , recordConstructor :: String
    , recordSize        :: Int
    , recordFieldNames  :: Rep (K String) a
    }

{-------------------------------------------------------------------------------
  Some specialised instances for 'Rep
-------------------------------------------------------------------------------}

instance Show x => Show (Rep (K x) a) where
  show (Rep v) = show $ map unK (V.toList v)

instance Eq x => Eq (Rep (K x) a) where
  Rep v == Rep v' = map unK (V.toList v) == map unK (V.toList v')
