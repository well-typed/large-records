{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms    #-}

-- | Supporting definitions used by both the simple and the advanced interface
--
-- To use the anonymous records library, you will want to use either the simple
-- interface in "Data.Record.Anon.Simple" or the advanced interface in
-- "Data.Record.Anon.Advanced". /This/ module provides definitions that are used
-- by both. Moreover, unlike @.Simple@ and @.Advanced@, this module is designed
-- to be imported unqualified. A typical import section will therefore look
-- something like
--
-- > import Data.Record.Anon
-- > import Data.Record.Anon.Simple (Record)
-- > import qualified Data.Record.Anon.Simple as Anon
--
-- In addition, since the classes and type families defined here as handled by
-- the plugin, you will also want to enable that:
--
-- > {-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}
module Data.Record.Anon (
    -- * Rows
    pattern (:=)
  , Row
  , Merge

    -- * Constraints
  , AllFields
  , KnownFields
  , SubRow
  , RowHasField

    -- * Fields
  , Field
  , KnownHash(..)

    -- * Type-level metadata
    --
    -- | This is primarily for interop with @generics-sop@.
  , FieldTypes
  , SimpleFieldTypes

    -- * Type utilities
  , Some(..)
  , Reflected(..)

    -- * Re-exports
  , module Data.Functor.Product
  , module Data.Proxy
  , module Data.SOP.BasicFunctors
  , module Data.SOP.Classes
  , module Data.SOP.Constraint
  , module Data.SOP.Dict
  , module GHC.Records.Compat
  , module GHC.TypeLits
  ) where

-- We use explicit import lists for all modules we re-export, so that
-- Haddock knows what to include in the documentation (and moreover we don't
-- end up exporting something unexpected).

import Data.Functor.Product (Product(..))
import Data.Proxy (Proxy(..))
import Data.SOP.BasicFunctors (I(..), K(..), (:.:)(..), unI)
import Data.SOP.Classes (type (-.->)(..))
import Data.SOP.Constraint (Compose)
import Data.SOP.Dict (Dict(..))
import GHC.Records.Compat (HasField(..))
import GHC.TypeLits (KnownSymbol)

import Data.Record.Anon.Plugin.Internal.Runtime
import Data.Record.Anon.Internal.Advanced (Field, Some(..))
import Data.Record.Anon.Internal.Reflection (Reflected(Reflected))

