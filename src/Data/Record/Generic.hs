{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ExplicitNamespaces      #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Record.Generic (
    -- * Types with a generic view
    Generic(..)
  , Rep(..)
    -- * Metadata
  , Metadata(..)
  , FieldName
    -- * Re-exports from sop-core
  , module SOP
  ) where

import Data.Kind
import Data.Proxy
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Exts (Any)

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
  type Constraints a :: (Type -> Constraint) -> Constraint

  -- | Translate to generic representation
  from :: a -> Rep I a

  -- | Translate from generic representation
  to :: Rep I a -> a

  -- | Number of fields in the record
  recordSize :: Proxy a -> Int

  -- | Construct vector of dictionaries, one for each field of the record
  dict :: Constraints a c => Proxy c -> Rep (Dict c) a

-- | Representation of some record @a@
--
-- The @f@ parameter describes which functor has been applied to all fields of
-- the record; in other words @Rep I@ is isomorphic to the record itself.
newtype Rep f a = Rep (Vector (f Any))

{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

class Generic a => Metadata a where
  metadata :: proxy a -> Rep (K FieldName) a

type FieldName = Text

{-------------------------------------------------------------------------------
  Some specialised instances for 'Rep
-------------------------------------------------------------------------------}

instance Show x => Show (Rep (K x) a) where
  show (Rep v) = show $ map unK (V.toList v)

instance Eq x => Eq (Rep (K x) a) where
  Rep v == Rep v' = map unK (V.toList v) == map unK (V.toList v')
