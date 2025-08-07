{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Record.Internal.Plugin.Names (
    QualifiedNames(..)
  , getQualifiedNames
  ) where

import Prelude hiding (error)
import Data.Record.Internal.GHC.Shim

{-------------------------------------------------------------------------------
  Qualified names
-------------------------------------------------------------------------------}

data QualifiedNames = QualifiedNames {

      --
      -- Prelude type classes
      --

      prelude_type_Eq   :: LRdrName
    , prelude_type_Ord  :: LRdrName
    , prelude_type_Show :: LRdrName
    , prelude_compare   :: LRdrName
    , prelude_eq        :: LRdrName
    , prelude_showsPrec :: LRdrName

      --
      -- Other base
      --

    , type_Constraint  :: LRdrName
    , type_GHC_Generic :: LRdrName
    , type_GHC_Rep     :: LRdrName
    , type_Int         :: LRdrName
    , type_Proxy       :: LRdrName
    , type_Type        :: LRdrName
    , error            :: LRdrName
    , ghc_from         :: LRdrName
    , ghc_to           :: LRdrName
    , proxy            :: LRdrName

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

#if __GLASGOW_HASKELL__ >= 902
    , type_HasField :: LRdrName
    , getField      :: LRdrName
#endif

    , type_HasFieldCompat :: LRdrName
    , hasFieldCompat      :: LRdrName
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
    -- Prelude classes
    --
    -- Annoyingly, we cannot re-rexport these through our runtime module, since
    -- we cannot declare instances of type aliased classes.
    --

    prelude_type_Eq   <- exact <$> lookupTcName  ghcClasses (Just "ghc-prim") "Eq"
    prelude_type_Ord  <- exact <$> lookupTcName  ghcClasses (Just "ghc-prim") "Ord"
    prelude_type_Show <- exact <$> lookupTcName  ghcShow    Nothing           "Show"
    prelude_compare   <- exact <$> lookupVarName ghcClasses (Just "ghc-prim") "compare"
    prelude_eq        <- exact <$> lookupVarName ghcClasses (Just "ghc-prim") "=="
    prelude_showsPrec <- exact <$> lookupVarName ghcShow    Nothing           "showsPrec"

    --
    -- Other base
    --

    type_Constraint  <- exact <$> lookupTcName  runtime     Nothing "Constraint"
    type_GHC_Generic <- exact <$> lookupTcName  ghcGenerics Nothing "Generic"
    type_GHC_Rep     <- exact <$> lookupTcName  ghcGenerics Nothing "Rep"
    type_Proxy       <- exact <$> lookupTcName  runtime     Nothing "Proxy"
    type_Type        <- exact <$> lookupTcName  runtime     Nothing "Type"
    type_Int         <- exact <$> lookupTcName  runtime     Nothing "Int"
    error            <- exact <$> lookupVarName runtime     Nothing "error"
    ghc_from         <- exact <$> lookupVarName ghcGenerics Nothing "from"
    ghc_to           <- exact <$> lookupVarName ghcGenerics Nothing "to"
    proxy            <- exact <$> lookupVarName runtime     Nothing "proxy"

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

    -- GHC.Records.HasField

#if __GLASGOW_HASKELL__ >= 902
    type_HasField <- exact <$> lookupTcName  recordHasField Nothing "HasField"
    getField      <- exact <$> lookupVarName recordHasField Nothing "getField"
#endif

    --
    -- record-hasfield
    --

    type_HasFieldCompat <- exact <$> lookupTcName  recordHasFieldCompat (Just "record-hasfield") "HasField"
    hasFieldCompat      <- exact <$> lookupVarName recordHasFieldCompat (Just "record-hasfield") "hasField"

    return QualifiedNames{..}

  where
   exact :: Name -> LRdrName
   exact = noLoc . Exact

   ghcClasses, ghcShow :: ModuleName
   ghcClasses = mkModuleName "GHC.Classes"
   ghcShow    = mkModuleName "GHC.Show"

   runtime, recordHasField, ghcGenerics, largeGenerics :: ModuleName
   runtime        = mkModuleName "Data.Record.Plugin.Runtime"
   recordHasField = mkModuleName "GHC.Records"
   recordHasFieldCompat = mkModuleName "GHC.Records.Compat"
   ghcGenerics    = mkModuleName "GHC.Generics"
   largeGenerics  = mkModuleName "Data.Record.Generic"
