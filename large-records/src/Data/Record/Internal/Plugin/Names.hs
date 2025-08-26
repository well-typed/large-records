{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Record.Internal.Plugin.Names (
    QualifiedNames(..)
  , getQualifiedNames
  ) where

import Prelude hiding (error)
import Data.Record.Internal.GHC.Shim

import qualified Data.Record.Plugin.Runtime as Runtime
import qualified GHC.Generics
import qualified Data.Record.Generic
import qualified GHC.Records.Compat
import qualified Optics.Core
import qualified Optics.Label

{-------------------------------------------------------------------------------
  Qualified names
-------------------------------------------------------------------------------}

data QualifiedNames = QualifiedNames {

      --
      -- Prelude type classes
      --

      prelude_type_Eq   :: LIdP GhcPs
    , prelude_type_Ord  :: LIdP GhcPs
    , prelude_type_Show :: LIdP GhcPs
    , prelude_compare   :: LIdP GhcPs
    , prelude_eq        :: LIdP GhcPs
    , prelude_showsPrec :: LIdP GhcPs

      --
      -- Other base
      --

    , type_Constraint  :: LIdP GhcPs
    , type_GHC_Generic :: LIdP GhcPs
    , type_GHC_Rep     :: LIdP GhcPs
    , type_Int         :: LIdP GhcPs
    , type_Proxy       :: LIdP GhcPs
    , type_Type        :: LIdP GhcPs
    , error            :: LIdP GhcPs
    , ghc_from         :: LIdP GhcPs
    , ghc_to           :: LIdP GhcPs
    , proxy            :: LIdP GhcPs

      --
      -- AnyArray
      --

    , type_AnyArray    :: LIdP GhcPs
    , anyArrayFromList :: LIdP GhcPs
    , anyArrayToList   :: LIdP GhcPs
    , anyArrayIndex    :: LIdP GhcPs
    , anyArrayUpdate   :: LIdP GhcPs

      --
      -- large-generics
      --

    , type_LR_Generic     :: LIdP GhcPs
    , type_LR_MetadataOf  :: LIdP GhcPs
    , type_LR_Constraints :: LIdP GhcPs
    , lr_from             :: LIdP GhcPs
    , lr_to               :: LIdP GhcPs
    , lr_dict             :: LIdP GhcPs
    , lr_metadata         :: LIdP GhcPs

      -- .. wrappers

    , type_Rep         :: LIdP GhcPs
    , type_Dict        :: LIdP GhcPs
    , gcompare         :: LIdP GhcPs
    , geq              :: LIdP GhcPs
    , gshowsPrec       :: LIdP GhcPs
    , noInlineUnsafeCo :: LIdP GhcPs

      -- .. utilities

    , anyArrayToRep   :: LIdP GhcPs
    , anyArrayFromRep :: LIdP GhcPs
    , mkDicts         :: LIdP GhcPs
    , mkDict          :: LIdP GhcPs
    , mkStrictField   :: LIdP GhcPs
    , mkLazyField     :: LIdP GhcPs
    , mkMetadata      :: LIdP GhcPs

      -- .. ThroughLRGenerics

    , type_ThroughLRGenerics  :: LIdP GhcPs
    , wrapThroughLRGenerics   :: LIdP GhcPs
    , unwrapThroughLRGenerics :: LIdP GhcPs

      --
      -- record-hasfield
      --

    , type_HasField :: LIdP GhcPs
    , hasField      :: LIdP GhcPs

      -- optics

    , type_LabelOptic :: LIdP GhcPs
    , type_A_Lens     :: LIdP GhcPs
    , labelOptic      :: LIdP GhcPs
    , lens            :: LIdP GhcPs
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

    prelude_type_Eq   <- exact <$> thNameToGhcNameHsc ''Eq
    prelude_type_Ord  <- exact <$> thNameToGhcNameHsc ''Ord
    prelude_type_Show <- exact <$> thNameToGhcNameHsc ''Show
    prelude_compare   <- exact <$> thNameToGhcNameHsc 'compare
    prelude_eq        <- exact <$> thNameToGhcNameHsc '(==)
    prelude_showsPrec <- exact <$> thNameToGhcNameHsc 'showsPrec

    --
    -- Other base
    --

    type_Constraint  <- exact <$> thNameToGhcNameHsc ''Runtime.Constraint
    type_GHC_Generic <- exact <$> thNameToGhcNameHsc ''GHC.Generics.Generic
    type_GHC_Rep     <- exact <$> thNameToGhcNameHsc ''GHC.Generics.Rep
    type_Proxy       <- exact <$> thNameToGhcNameHsc ''Runtime.Proxy
    type_Type        <- exact <$> thNameToGhcNameHsc ''Runtime.Type
    type_Int         <- exact <$> thNameToGhcNameHsc ''Runtime.Int
    error            <- exact <$> thNameToGhcNameHsc 'Runtime.error
    ghc_from         <- exact <$> thNameToGhcNameHsc 'GHC.Generics.from
    ghc_to           <- exact <$> thNameToGhcNameHsc 'GHC.Generics.to
    proxy            <- exact <$> thNameToGhcNameHsc 'Runtime.proxy

    --
    -- AnyArray
    --

    type_AnyArray    <- exact <$> thNameToGhcNameHsc ''Runtime.AnyArray
    anyArrayFromList <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayFromList
    anyArrayToList   <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayToList
    anyArrayIndex    <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayIndex
    anyArrayUpdate   <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayUpdate

    --
    -- large-generics
    --

    type_LR_Generic     <- exact <$> thNameToGhcNameHsc ''Data.Record.Generic.Generic
    type_LR_Constraints <- exact <$> thNameToGhcNameHsc ''Data.Record.Generic.Constraints
    type_LR_MetadataOf  <- exact <$> thNameToGhcNameHsc ''Data.Record.Generic.MetadataOf
    lr_from             <- exact <$> thNameToGhcNameHsc 'Data.Record.Generic.from
    lr_to               <- exact <$> thNameToGhcNameHsc 'Data.Record.Generic.to
    lr_dict             <- exact <$> thNameToGhcNameHsc 'Data.Record.Generic.dict
    lr_metadata         <- exact <$> thNameToGhcNameHsc 'Data.Record.Generic.metadata

    -- .. utilities

    anyArrayToRep   <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayToRep
    anyArrayFromRep <- exact <$> thNameToGhcNameHsc 'Runtime.anyArrayFromRep
    mkDicts         <- exact <$> thNameToGhcNameHsc 'Runtime.mkDicts
    mkDict          <- exact <$> thNameToGhcNameHsc 'Runtime.mkDict
    mkStrictField   <- exact <$> thNameToGhcNameHsc 'Runtime.mkStrictField
    mkLazyField     <- exact <$> thNameToGhcNameHsc 'Runtime.mkLazyField
    mkMetadata      <- exact <$> thNameToGhcNameHsc 'Runtime.mkMetadata

    -- .. wrappers

    type_Rep         <- exact <$> thNameToGhcNameHsc ''Runtime.Rep
    type_Dict        <- exact <$> thNameToGhcNameHsc ''Runtime.Dict
    gcompare         <- exact <$> thNameToGhcNameHsc 'Runtime.gcompare
    geq              <- exact <$> thNameToGhcNameHsc 'Runtime.geq
    gshowsPrec       <- exact <$> thNameToGhcNameHsc 'Runtime.gshowsPrec
    noInlineUnsafeCo <- exact <$> thNameToGhcNameHsc 'Runtime.noInlineUnsafeCo

    -- .. ThroughLRGenerics

    type_ThroughLRGenerics  <- exact <$> thNameToGhcNameHsc ''Runtime.ThroughLRGenerics
    wrapThroughLRGenerics   <- exact <$> thNameToGhcNameHsc 'Runtime.wrapThroughLRGenerics
    unwrapThroughLRGenerics <- exact <$> thNameToGhcNameHsc 'Runtime.unwrapThroughLRGenerics

    --
    -- record-hasfield
    --

    type_HasField <- exact <$> thNameToGhcNameHsc ''GHC.Records.Compat.HasField
    hasField      <- exact <$> thNameToGhcNameHsc 'GHC.Records.Compat.hasField

    -- optics

    type_LabelOptic <- exact <$> thNameToGhcNameHsc ''Optics.Label.LabelOptic
    labelOptic      <- exact <$> thNameToGhcNameHsc 'Optics.Label.labelOptic
    type_A_Lens     <- exact <$> thNameToGhcNameHsc ''Optics.Core.A_Lens
    lens            <- exact <$> thNameToGhcNameHsc 'Optics.Core.lens

    return QualifiedNames{..}

  where
   exact :: Name -> LIdP GhcPs
   exact = noLocA . Exact
