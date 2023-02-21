{-# LANGUAGE CPP #-}

-- | Thin shim around the GHC API
--
-- For the typechecker part we have the excellent @ghc-tcplugin-api@ library;
-- unfortunately, we have no such library for source plugins. We could reuse a
-- small part of @ghc-tcplugin-api@ here, but there isn't too much point: source
-- plugins need quite a different subset of the GHC API than typechecker plugins
-- do.
module Data.Record.Anon.Internal.Plugin.Source.GhcShim (
    -- * Extensions
    HasDefaultExt(..)

#if __GLASGOW_HASKELL__ < 902
    -- * Exact-print annotations
  , reLoc, reLocA
#endif

    -- * Names
  , lookupName

    -- * Miscellaneous
  , importDecl
  , issueWarning
  , mkLabel
#if __GLASGOW_HASKELL__ < 900
  , mkHsApps
#endif

    -- * Re-exports
#if __GLASGOW_HASKELL__ < 900
  , module BasicTypes
  , module FastString
  , module GHC
  , module HscMain
  , module HscTypes
  , module Name
  , module NameCache
  , module OccName
  , module Outputable
  , module RdrName
  , module UniqSupply
#else
  , module GHC
  , module GHC.Data.FastString
  , module GHC.Driver.Main
  , module GHC.Types.Name
  , module GHC.Types.Name.Cache
  , module GHC.Types.Name.Occurrence
  , module GHC.Types.Name.Reader
  , module GHC.Types.Unique.Supply
  , module GHC.Utils.Outputable

#if __GLASGOW_HASKELL__ < 902
  , module GHC.Driver.Types
#else
  , module GHC.Driver.Errors
  , module GHC.Driver.Env.Types
  , module GHC.Types.SourceText
#endif
#endif
  ) where

import GHC.Stack

#if __GLASGOW_HASKELL__ < 900

import Data.List (foldl')

import GHC hiding (lookupName)

import Bag (listToBag)
import BasicTypes (Origin(Generated), PromotionFlag(NotPromoted), SourceText(NoSourceText))
import DynFlags (getDynFlags)
import ErrUtils (mkWarnMsg)
import FastString (FastString)
import Finder (findImportedModule)
import HscMain (getHscEnv)
import HscTypes
import IfaceEnv (lookupOrigIO)
import MonadUtils
import Name (mkInternalName)
import NameCache (NameCache(nsUniqs))
import OccName
import Outputable
import RdrName (RdrName(Exact), rdrNameOcc, mkRdrQual, mkRdrUnqual)
import UniqSupply (takeUniqFromSupply)

#else

#if __GLASGOW_HASKELL__ >= 902
import GHC.Driver.Env.Types
import GHC.Driver.Errors
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Unit.Finder (findImportedModule, FindResult(Found))
import GHC.Unit.Types (IsBootInterface(NotBoot))
#else
import GHC.Driver.Finder (findImportedModule)
import GHC.Driver.Types
import GHC.Types.Basic (SourceText(NoSourceText))
#endif

import GHC hiding (lookupName)

import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (FastString)
import GHC.Driver.Main (getHscEnv)
import GHC.Driver.Session (getDynFlags)
import GHC.Iface.Env (lookupOrigIO)
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Cache (NameCache(nsUniqs))
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader (RdrName(Exact), rdrNameOcc, mkRdrQual, mkRdrUnqual)
import GHC.Types.SrcLoc (LayoutInfo(NoLayoutInfo))
import GHC.Types.Unique.Supply (takeUniqFromSupply)
import GHC.Utils.Error (mkWarnMsg)
import GHC.Utils.Monad
import GHC.Utils.Outputable

#endif

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

lookupName ::
     HasCallStack
  => ModuleName
  -> Maybe FastString -- ^ Optional package name
  -> String -> Hsc Name
lookupName modl pkg = lookupOccName modl pkg . mkVarOcc

lookupOccName ::
     HasCallStack
  => ModuleName
  -> Maybe FastString -- ^ Optional package name
  -> OccName -> Hsc Name
lookupOccName modlName mPkgName name = do
    env   <- getHscEnv
    mModl <- liftIO $ findImportedModule env modlName mPkgName
    case mModl of
      Found _ modl -> liftIO $ lookupOrigIO env modl name
      _otherwise   -> error $ "lookupName: name not found"

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: Bool -> ModuleName -> LImportDecl GhcPs
importDecl qualified name = reLocA $ noLoc $ ImportDecl {
      ideclExt       = defExt
    , ideclSourceSrc = NoSourceText
    , ideclName      = reLocA $ noLoc name
    , ideclPkgQual   = Nothing
    , ideclSafe      = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Nothing
#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = qualified
#else
    , ideclQualified = if qualified then QualifiedPre else NotQualified
#endif
#if __GLASGOW_HASKELL__ < 900
    , ideclSource    = False
#else
    , ideclSource    = NotBoot
#endif
    }

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
    dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ >= 902
    logger <- getLogger
    liftIO $ printOrThrowWarnings logger dynFlags . listToBag . (:[]) $
      mkWarnMsg l neverQualify errMsg
#else
    liftIO $ printOrThrowWarnings dynFlags . listToBag . (:[]) $
      mkWarnMsg dynFlags l neverQualify errMsg
#endif

#if __GLASGOW_HASKELL__ < 900
mkHsApps ::
     LHsExpr (GhcPass id)
  -> [LHsExpr (GhcPass id)]
  -> LHsExpr (GhcPass id)
mkHsApps = foldl' mkHsApp
#endif

{-------------------------------------------------------------------------------
  Extensions
-------------------------------------------------------------------------------}

class HasDefaultExt a where
  defExt :: a

#if __GLASGOW_HASKELL__ < 810
instance HasDefaultExt NoExt where
  defExt = noExt
#else
instance HasDefaultExt NoExtField where
  defExt = noExtField
#endif

#if __GLASGOW_HASKELL__ >= 900
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
#endif

#if __GLASGOW_HASKELL__ >= 902
instance HasDefaultExt (EpAnn ann) where
  defExt = noAnn
#endif

{-------------------------------------------------------------------------------
  Exact-print annotations
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ < 902
reLoc :: Located a -> Located a
reLoc = id

reLocA :: Located a -> Located a
reLocA = id
#endif

{-------------------------------------------------------------------------------
  mkLabel
-------------------------------------------------------------------------------}

mkLabel :: SrcSpan -> FastString -> LHsExpr GhcPs
mkLabel l n = reLocA $ L l
            $ HsOverLabel defExt
#if __GLASGOW_HASKELL__ < 902
                 Nothing
#endif
                 n
