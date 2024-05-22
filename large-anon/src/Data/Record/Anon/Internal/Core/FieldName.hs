{-# LANGUAGE CPP #-}

-- | Field name
--
-- Intended for qualified import.
--
-- > import Data.Record.Anon.Internal.Core.FieldName (FieldName(..))
-- > import qualified Data.Record.Anon.Internal.Core.FieldName as FieldName
module Data.Record.Anon.Internal.Core.FieldName (
    -- * Definition
    FieldName(..)
    -- * Conversion
  , fromString
  , fromFastString
  ) where

import Data.Hashable
import Data.String

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI

#if __GLASGOW_HASKELL__ >= 906
import GHC.Data.FastString (mkFastString)
#endif


{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Record field name
--
-- A fieldname carries its own hash, which is computed by the plugin at
-- compile time.
data FieldName = FieldName {
      fieldNameHash  :: Int
    , fieldNameLabel :: String
    }
  -- For the 'Eq' and 'Ord' instances it's important the 'Int' comes first
  deriving (Eq, Ord)

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | From 'FastString'
--
-- This happens at compile time in the plugin.
fromFastString :: FastString -> FieldName
fromFastString = fromString . unpackFS

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Convenience constructor for 'FieldName'
--
-- This function is primarily for use in the 'Show' instance and for debugging.
-- Other applications should use 'fieldNameVal' instead, so that the hash
-- is computed at compile time instead.
instance IsString FieldName where
  fromString str = FieldName (hash str) str

instance Hashable FieldName where
  hash           = fieldNameHash
  hashWithSalt s = hashWithSalt s . fieldNameHash

instance Show FieldName where
  showsPrec p n = showParen (p >= 11) $
    showString "fromString " . showsPrec 11 (fieldNameLabel n)

instance Outputable FieldName where
#if __GLASGOW_HASKELL__ >= 906
  ppr = ppr . mkFastString . fieldNameLabel
#else
  ppr = ppr . fieldNameLabel
#endif
