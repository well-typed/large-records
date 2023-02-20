{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Record.Internal.Plugin.Names (
    -- * Qualified names
    QualifiedNames(..)
  , getQualifiedNames
    -- * Unqualified names
  , UnqualifiedNames(..)
  , getUnqualifiedNames
  ) where

import Data.Record.Internal.GHC.Shim

{-------------------------------------------------------------------------------
  Qualified names
-------------------------------------------------------------------------------}

data QualifiedNames = QualifiedNames {

      --
      -- Base
      --

      type_Constraint  :: LRdrName
    , type_GHC_Generic :: LRdrName
    , type_GHC_Rep     :: LRdrName
    , type_Proxy       :: LRdrName
    , type_Type        :: LRdrName
    , proxy            :: LRdrName
    , ghc_from         :: LRdrName
    , ghc_to           :: LRdrName

      --
      -- AnyArray
      --

    , type_AnyArray    :: LRdrName
    , anyArrayFromList :: LRdrName
    , anyArrayToList   :: LRdrName
    , anyArrayIndex    :: LRdrName
    , anyArrayUpdate   :: LRdrName

      --
      -- large-generics
      --

    , type_LR_Generic     :: LRdrName
    , type_LR_MetadataOf  :: LRdrName
    , type_LR_Constraints :: LRdrName
    , lr_from             :: LRdrName
    , lr_to               :: LRdrName
    , lr_dict             :: LRdrName
    , lr_metadata         :: LRdrName

      -- .. wrappers

    , type_Rep         :: LRdrName
    , type_Dict        :: LRdrName
    , gcompare         :: LRdrName
    , geq              :: LRdrName
    , gshowsPrec       :: LRdrName
    , noInlineUnsafeCo :: LRdrName

      -- .. utilities

    , anyArrayToRep   :: LRdrName
    , anyArrayFromRep :: LRdrName
    , mkDicts         :: LRdrName
    , mkDict          :: LRdrName
    , mkStrictField   :: LRdrName
    , mkLazyField     :: LRdrName
    , mkMetadata      :: LRdrName

      -- .. ThroughLRGenerics

    , type_ThroughLRGenerics  :: LRdrName
    , wrapThroughLRGenerics   :: LRdrName
    , unwrapThroughLRGenerics :: LRdrName

      --
      -- record-hasfield
      --

    , type_HasField :: LRdrName
    , hasField      :: LRdrName
    }

-- | Resolve qualified names
--
-- We try to import whenever possible from "Data.Record.Plugin.Runtime"; only
-- when this is /really/ not possible do we import from other modules. We do
-- this to avoid two problems:
--
-- * When we resolve a name, we must specify the module where something is
--   /defined/, not merely a module that /exports/ the thing we need; this means
--   that this is quite brittle.
--
-- * When we resolve a name from a different package, users must explicitly
--   define a dependency on that other package.
getQualifiedNames :: Hsc QualifiedNames
getQualifiedNames = do
    --
    -- base
    --

    type_Constraint  <- exact <$> lookupTcName  runtime     Nothing "Constraint"
    type_GHC_Generic <- exact <$> lookupTcName  ghcGenerics Nothing "Generic"
    type_GHC_Rep     <- exact <$> lookupTcName  ghcGenerics Nothing "Rep"
    type_Proxy       <- exact <$> lookupTcName  runtime     Nothing "Proxy"
    type_Type        <- exact <$> lookupTcName  runtime     Nothing "Type"
    proxy            <- exact <$> lookupVarName runtime     Nothing "proxy"
    ghc_from         <- exact <$> lookupVarName ghcGenerics Nothing "from"
    ghc_to           <- exact <$> lookupVarName ghcGenerics Nothing "to"

    --
    -- AnyArray
    --

    type_AnyArray    <- exact <$> lookupTcName  runtime Nothing "AnyArray"
    anyArrayFromList <- exact <$> lookupVarName runtime Nothing "anyArrayFromList"
    anyArrayToList   <- exact <$> lookupVarName runtime Nothing "anyArrayToList"
    anyArrayIndex    <- exact <$> lookupVarName runtime Nothing "anyArrayIndex"
    anyArrayUpdate   <- exact <$> lookupVarName runtime Nothing "anyArrayUpdate"

    --
    -- large-generics
    --

    type_LR_Generic     <- exact <$> lookupTcName  largeGenerics (Just "large-generics") "Generic"
    type_LR_Constraints <- exact <$> lookupTcName  largeGenerics (Just "large-generics") "Constraints"
    type_LR_MetadataOf  <- exact <$> lookupTcName  largeGenerics (Just "large-generics") "MetadataOf"
    lr_from             <- exact <$> lookupVarName largeGenerics (Just "large-generics") "from"
    lr_to               <- exact <$> lookupVarName largeGenerics (Just "large-generics") "to"
    lr_dict             <- exact <$> lookupVarName largeGenerics (Just "large-generics") "dict"
    lr_metadata         <- exact <$> lookupVarName largeGenerics (Just "large-generics") "metadata"

    -- .. utilities

    anyArrayToRep   <- exact <$> lookupVarName runtime Nothing "anyArrayToRep"
    anyArrayFromRep <- exact <$> lookupVarName runtime Nothing "anyArrayFromRep"
    mkDicts         <- exact <$> lookupVarName runtime Nothing "mkDicts"
    mkDict          <- exact <$> lookupVarName runtime Nothing "mkDict"
    mkStrictField   <- exact <$> lookupVarName runtime Nothing "mkStrictField"
    mkLazyField     <- exact <$> lookupVarName runtime Nothing "mkLazyField"
    mkMetadata      <- exact <$> lookupVarName runtime Nothing "mkMetadata"

    -- .. wrappers

    type_Rep         <- exact <$> lookupTcName  runtime Nothing "Rep"
    type_Dict        <- exact <$> lookupTcName  runtime Nothing "Dict"
    gcompare         <- exact <$> lookupVarName runtime Nothing "gcompare"
    geq              <- exact <$> lookupVarName runtime Nothing "geq"
    gshowsPrec       <- exact <$> lookupVarName runtime Nothing "gshowsPrec"
    noInlineUnsafeCo <- exact <$> lookupVarName runtime Nothing "noInlineUnsafeCo"

    -- .. ThroughLRGenerics

    type_ThroughLRGenerics  <- exact <$> lookupTcName  runtime Nothing "ThroughLRGenerics"
    wrapThroughLRGenerics   <- exact <$> lookupVarName runtime Nothing "wrapThroughLRGenerics"
    unwrapThroughLRGenerics <- exact <$> lookupVarName runtime Nothing "unwrapThroughLRGenerics"

    --
    -- record-hasfield
    --

    type_HasField <- exact <$> lookupTcName  recordHasField (Just "record-hasfield") "HasField"
    hasField      <- exact <$> lookupVarName recordHasField (Just "record-hasfield") "hasField"

    return QualifiedNames{..}

  where
   exact :: Name -> LRdrName
   exact = noLoc . Exact

   runtime, recordHasField, ghcGenerics, largeGenerics :: ModuleName
   runtime        = mkModuleName "Data.Record.Plugin.Runtime"
   recordHasField = mkModuleName "GHC.Records.Compat"
   ghcGenerics    = mkModuleName "GHC.Generics"
   largeGenerics  = mkModuleName "Data.Record.Generic"

{-------------------------------------------------------------------------------
  We use Prelude names unqualified.
-------------------------------------------------------------------------------}

data UnqualifiedNames = UnqualifiedNames {
      unq_type_Eq   :: LRdrName
    , unq_type_Int  :: LRdrName
    , unq_type_Ord  :: LRdrName
    , unq_type_Show :: LRdrName
    , unq_compare   :: LRdrName
    , unq_eq        :: LRdrName
    , unq_error     :: LRdrName
    , unq_showsPrec :: LRdrName
    }

getUnqualifiedNames :: UnqualifiedNames
getUnqualifiedNames = UnqualifiedNames {
      unq_type_Eq   = tc "Eq"
    , unq_type_Int  = tc "Int"
    , unq_type_Ord  = tc "Ord"
    , unq_type_Show = tc "Show"
    , unq_compare   = var "compare"
    , unq_eq        = var "=="
    , unq_error     = var "error"
    , unq_showsPrec = var "showsPrec"
    }
  where
    var, tc :: String -> LRdrName
    var x = noLoc $ mkRdrUnqual $ mkVarOcc x
    tc  x = noLoc $ mkRdrUnqual $ mkTcOcc  x
