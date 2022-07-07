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

#if __GLASGOW_HASKELL__ < 900

import Data.List (foldl')

import Bag (listToBag)
import BasicTypes (Origin(Generated), PromotionFlag(NotPromoted))
import ErrUtils (mkWarnMsg)
import FastString (FastString)
import GHC
import GhcPlugins
import HscMain (getHscEnv)
import HscTypes
import Name (mkInternalName)
import NameCache (NameCache(nsUniqs))
import OccName
import Outputable
import RdrName (RdrName(Exact), rdrNameOcc, mkRdrQual, mkRdrUnqual)
import UniqSupply (takeUniqFromSupply)

#else

import GHC
import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (FastString)
import GHC.Driver.Main (getHscEnv)

#if __GLASGOW_HASKELL__ >= 902
import GHC.Driver.Errors
import GHC.Driver.Env.Types
import GHC.Types.SourceText
#else
import GHC.Driver.Types
#endif
import GHC.Plugins
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Cache (NameCache(nsUniqs))
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader (RdrName(Exact), rdrNameOcc, mkRdrQual, mkRdrUnqual)
import GHC.Types.Unique.Supply (takeUniqFromSupply)
import GHC.Utils.Error (mkWarnMsg)
import GHC.Utils.Outputable

#endif

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
