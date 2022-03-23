{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

-- | Information about a field in a record
--
-- Intended for qualified import
--
-- > import Data.Record.Anonymous.Internal.Row.KnownField (KnownField(..))
-- > import qualified Data.Record.Anonymous.Row.Record.KnownField as KnownField
module Data.Record.Anonymous.Internal.Row.KnownField (
    -- * Definition
    KnownField(..)
    -- * Interop with @large-generics@
  , fromFieldMetadata
  ) where

import Data.Record.Generic (FieldMetadata(..))
import GHC.TypeLits (symbolVal)

import Data.Record.Anonymous.Internal.Row.FieldName (FieldName)
import Data.Record.Anonymous.Plugin.GhcTcPluginAPI

import qualified Data.Record.Anonymous.Internal.Row.FieldName as FieldName

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Context-free information about a field in a record
--
-- In other words, we do /not/ know the /index/ of the field here, as that
-- depends the context (the particular record it is part of).
data KnownField a = KnownField {
      knownFieldName :: FieldName
    , knownFieldInfo :: a
    }
  deriving (Functor, Foldable)

{-------------------------------------------------------------------------------
  Interop with @large-generics@
-------------------------------------------------------------------------------}

-- | Construct 'KnownField' from @large-generics@ metadata
--
-- NOTE: This currently involves a hash computation, since @large-generics@
-- does not precompute those. We could change that in @large-generics@.
fromFieldMetadata :: FieldMetadata a -> KnownField ()
fromFieldMetadata (FieldMetadata p _) = KnownField {
      knownFieldName = FieldName.fromString $ symbolVal p
    , knownFieldInfo = ()
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable a => Outputable (KnownField a) where
  ppr (KnownField name info) = parens $
          text "KnownField"
      <+> ppr name
      <+> ppr info

