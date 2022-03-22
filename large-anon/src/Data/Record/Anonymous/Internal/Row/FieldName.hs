{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | Field names
--
-- Intended for qualified import
--
-- > import Data.Record.Anonymous.Internal.Row.FieldName (FieldName)
-- > import qualified Data.Record.Anonymous.Internal.Row.FieldName as FieldName
--
-- TODO: We should separate out the user facing code (like KnownHash) and the
-- internal code from this module.
module Data.Record.Anonymous.Internal.Row.FieldName (
    FieldName(..)
    -- * Conversion
  , fromString
  , fromFastString
    -- * Construction
  , KnownHash(..)
  , DictKnownHash
  , symbolVal
    -- * Code generation
  , mkExpr
  , mkType
  ) where

import Data.Hashable
import Data.Proxy
import GHC.TypeLits (Symbol, KnownSymbol)

import qualified GHC.TypeLits as GHC

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution

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
  deriving (Eq)

instance Hashable FieldName where
  hash           = fieldNameHash
  hashWithSalt s = hashWithSalt s . fieldNameHash

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

-- | Convenience constructor for 'FieldName'
--
-- This function is primarily for use in the 'Show' instance and for debugging.
-- Other applications should use 'fieldNameVal' instead, so that the hash
-- is computed at compile time instead.
fromString :: String -> FieldName
fromString str = FieldName (hash str) str

-- | From 'FastString'
--
-- This happens at compile time in the plugin.
fromFastString :: FastString -> FieldName
fromFastString = fromString . unpackFS

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Show FieldName where
  showsPrec p n = showParen (p >= 11) $
    showString "fromString " . showString (fieldNameLabel n)

instance Outputable FieldName where
  ppr = ppr . fieldNameLabel

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Symbol (type-level string) with compile-time computed hash
--
-- Instances are computed on the fly by the plugin.
class KnownHash (s :: Symbol) where
  hashVal :: DictKnownHash s

type DictKnownHash (s :: Symbol) = Proxy s -> Int

-- | Compile-time construction of a 'FieldName'
symbolVal :: (KnownSymbol s, KnownHash s) => Proxy s -> FieldName
symbolVal p = FieldName (hashVal p) (GHC.symbolVal p)

{-------------------------------------------------------------------------------
  Code generation
-------------------------------------------------------------------------------}

-- | Core generation that would result in this 'FieldName'
mkExpr :: MonadThings m => ResolvedNames -> FieldName -> m CoreExpr
mkExpr ResolvedNames{..} FieldName{..} = do
    str <- mkStringExpr fieldNameLabel
    return $
      mkCoreConApps
        dataConFieldName
        [ mkUncheckedIntExpr (fromIntegral fieldNameHash)
        , str
        ]

-- | Promote to the type-level
mkType :: FieldName -> Type
mkType = mkStrLitTy . fsLit . fieldNameLabel

