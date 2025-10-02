{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

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
#if __GLASGOW_HASKELL__ >= 910
  , reLocA
#endif

    -- * Names
  , lookupName

    -- * NameCache
  , NameCacheIO
  , hscNameCacheIO
  , takeUniqFromNameCacheIO

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
  , module OccName
  , module Outputable
  , module RdrName
  , module UniqSupply
#else
  , module GHC
  , module GHC.Data.FastString
  , module GHC.Driver.Main
  , module GHC.Types.Name
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

import Data.IORef
import Data.List (foldl')

import GHC hiding (lookupName)

import Bag (Bag, listToBag)
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
import Unique (Unique)

#else

import GHC hiding (lookupName)

import GHC.Data.Bag (listToBag, Bag)
import GHC.Data.FastString (FastString)
import GHC.Driver.Main (getHscEnv)
import GHC.Driver.Session (getDynFlags)
import GHC.Types.Name (mkInternalName)
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader (RdrName(Exact), rdrNameOcc, mkRdrQual, mkRdrUnqual)
import GHC.Types.Unique (Unique)
import GHC.Types.Unique.Supply (takeUniqFromSupply)
import GHC.Utils.Monad
import GHC.Utils.Outputable

#if __GLASGOW_HASKELL__ >= 902
import GHC.Driver.Env.Types
import GHC.Driver.Errors
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Unit.Finder (findImportedModule, FindResult(Found))
#else
import GHC.Driver.Finder (findImportedModule)
import GHC.Driver.Types
import GHC.Types.Basic (SourceText(NoSourceText))
#endif

#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 906
import GHC.Unit.Types (IsBootInterface(NotBoot))
#endif

#if __GLASGOW_HASKELL__ < 904
import Data.IORef

import GHC.Iface.Env (lookupOrigIO)
import GHC.Types.Name.Cache (NameCache(nsUniqs))
import GHC.Utils.Error (mkWarnMsg)
#else
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors.Types (GhcMessage(..))
import GHC.Iface.Env (lookupNameCache)
import GHC.Rename.Names (renamePkgQual)
import GHC.Types.Name.Cache (NameCache, takeUniqFromNameCache)
import GHC.Types.PkgQual (RawPkgQual(NoRawPkgQual))
import GHC.Utils.Error (mkPlainError)
import qualified GHC.Types.Error as Err
#endif

#endif

#if __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 906
import GHC.Types.SrcLoc (LayoutInfo(NoLayoutInfo))
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Driver.Config.Diagnostic (initPrintConfig)
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
    env <- getHscEnv

#if __GLASGOW_HASKELL__ >= 904
    let pkgq :: PkgQual
        pkgq = renamePkgQual (hsc_unit_env env) modlName mPkgName
#else
    let pkgq :: Maybe FastString
        pkgq = mPkgName
#endif

    mModl <- liftIO $ findImportedModule env modlName pkgq
    case mModl of
      Found _ modl -> liftIO $ lookupOrigIO env modl name
      _otherwise   -> error $ "lookupName: name not found"

#if __GLASGOW_HASKELL__ >= 904
lookupOrigIO :: HscEnv -> Module -> OccName -> IO Name
lookupOrigIO env modl occ = lookupNameCache (hsc_NC env) modl occ
#endif

{-------------------------------------------------------------------------------
  NameCache
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ < 904
type NameCacheIO = IORef NameCache

takeUniqFromNameCacheIO :: NameCacheIO -> IO Unique
takeUniqFromNameCacheIO = flip atomicModifyIORef aux
  where
    aux :: NameCache -> (NameCache, Unique)
    aux nc = let (newUniq, us) = takeUniqFromSupply (nsUniqs nc)
             in (nc { nsUniqs = us }, newUniq)
#else
type NameCacheIO = NameCache

takeUniqFromNameCacheIO :: NameCacheIO -> IO Unique
takeUniqFromNameCacheIO = takeUniqFromNameCache
#endif

hscNameCacheIO :: HscEnv -> NameCacheIO
hscNameCacheIO = hsc_NC

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: Bool -> ModuleName -> LImportDecl GhcPs
importDecl qualified name = reLocA $ noLoc $ ImportDecl {
#if __GLASGOW_HASKELL__ < 906
      ideclExt       = defExt
#else
      ideclExt       = XImportDeclPass {
                           ideclAnn        = defExt
                         , ideclSourceText = NoSourceText
                         , ideclImplicit   = False
                         }
#endif
#if __GLASGOW_HASKELL__ < 906
    , ideclSourceSrc = NoSourceText
#endif
    , ideclName      = reLocA $ noLoc name
#if __GLASGOW_HASKELL__ >= 904
    , ideclPkgQual   = NoRawPkgQual
#else
    , ideclPkgQual   = Nothing
#endif
    , ideclSafe      = False
#if __GLASGOW_HASKELL__ < 906
    , ideclImplicit  = False
#endif
    , ideclAs        = Nothing
#if __GLASGOW_HASKELL__ < 906
    , ideclHiding    = Nothing
#endif
    , ideclQualified = if qualified then QualifiedPre else NotQualified
#if __GLASGOW_HASKELL__ < 900
    , ideclSource    = False
#else
    , ideclSource    = NotBoot
#endif
#if __GLASGOW_HASKELL__ >= 906
    , ideclImportList = Nothing
#endif
#if __GLASGOW_HASKELL__ >= 914
    , ideclLevelSpec = NotLevelled
#endif
    }

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
    dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ == 902
    logger <- getLogger
    liftIO $ printOrThrowWarnings logger dynFlags . bag $
      mkWarnMsg l neverQualify errMsg
#elif __GLASGOW_HASKELL__ >= 904
    logger <- getLogger

#if __GLASGOW_HASKELL__ < 906
    let printOrThrow :: Err.Messages GhcMessage -> IO ()
        printOrThrow = printOrThrowDiagnostics
                         logger
                         (initDiagOpts dynFlags)

    let msg :: Err.DiagnosticMessage
        msg = mkPlainError [] errMsg
#elif __GLASGOW_HASKELL__ < 908
    let printOrThrow :: Err.Messages GhcMessage -> IO ()
        printOrThrow = printOrThrowDiagnostics
                         logger
                         (initPrintConfig dynFlags)
                         (initDiagOpts dynFlags)

    let msg :: Err.UnknownDiagnostic
        msg = Err.UnknownDiagnostic $
                mkPlainError [] errMsg
#else
    let printOrThrow :: Err.Messages GhcMessage -> IO ()
        printOrThrow = printOrThrowDiagnostics
                         logger
                         (initPrintConfig dynFlags)
                         (initDiagOpts dynFlags)

#if __GLASGOW_HASKELL__ >= 914
    let msg :: Err.UnknownDiagnostic opts Err.GhcHint
#else
    let msg :: Err.UnknownDiagnostic opts
#endif
        msg = Err.mkSimpleUnknownDiagnostic $
                mkPlainError [] errMsg
#endif
    liftIO $ printOrThrow . Err.mkMessages . bag $
      Err.MsgEnvelope {
          errMsgSpan       = l
        , errMsgContext    = neverQualify
        , errMsgDiagnostic = GhcUnknownMessage msg
        , errMsgSeverity   = SevWarning
#if __GLASGOW_HASKELL__ >= 908
        , errMsgReason     = Err.ResolvedDiagnosticReason Err.WarningWithoutFlag
#endif
        }
#else
    liftIO $ printOrThrowWarnings dynFlags . bag $
      mkWarnMsg dynFlags l neverQualify errMsg
#endif
  where
    bag :: a -> Bag a
    bag = listToBag . (:[])

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

instance HasDefaultExt NoExtField where
  defExt = noExtField

#if __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 906
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
#endif

#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 910
instance HasDefaultExt (EpAnn ann) where
  defExt = noAnn
#elif __GLASGOW_HASKELL__ >= 910
instance NoAnn ann => HasDefaultExt (EpAnn ann) where
  defExt = noAnn
#endif

#if __GLASGOW_HASKELL__ >= 906
instance HasDefaultExt SourceText where
  defExt = NoSourceText
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

#if __GLASGOW_HASKELL__ >= 910
reLocA ::
     (HasLoc (GenLocated a e), HasAnnotation b)
  => GenLocated a e -> GenLocated b e
reLocA = reLoc
#endif

{-------------------------------------------------------------------------------
  mkLabel
-------------------------------------------------------------------------------}

mkLabel :: SrcSpan -> FastString -> LHsExpr GhcPs
mkLabel l n = reLocA $ L l
            $ HsOverLabel defExt
#if __GLASGOW_HASKELL__ < 902
                 Nothing -- RebindableSyntax
#elif __GLASGOW_HASKELL__ >= 906 && __GLASGOW_HASKELL__ < 912
                 defExt
#endif
                 n
