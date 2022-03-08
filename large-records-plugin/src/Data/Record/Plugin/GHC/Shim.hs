{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Plugin.GHC.Shim (
    -- * Miscellaneous
    qimportD
  , conPat
  , funBind
  , HsModule
  , pattern GHC.HsModule

    -- * Extensions
  , HasDefaultExt(..)

    -- * Generalized @forall@
#if __GLASGOW_HASKELL__ >= 900
  , HsTyVarBndr
  , LHsTyVarBndr
#endif
  , hsFunTy
  , userTyVar
  , kindedTyVar
  , setDefaultSpecificity

    -- * Re-exports

    -- The whole-sale module exports are not ideal for preserving compatibility
    -- across ghc versions, but we'll deal with this on a case by case basis.
    --
    -- TODO: Do we still need all of these, now that some stuff has moved here?
#if __GLASGOW_HASKELL__ < 900
  , module Bag
  , module BasicTypes
  , module GHC
  , module GhcPlugins
  , module TcEvidence
#else
  , module GHC.Data.Bag
  , module GHC.Hs
  , module GHC.Plugins
  , module GHC.Tc.Types.Evidence
  , module GHC.Types.Basic
#endif
  ) where

#if __GLASGOW_HASKELL__ < 900
import Bag (listToBag, emptyBag)
import BasicTypes (SourceText(NoSourceText))
import GHC hiding (AnnKeywordId(..), HsModule, exprType, typeKind)
import GhcPlugins hiding ((<>))
import TcEvidence (HsWrapper(WpHole))
import qualified GHC
#else
import GHC.Data.Bag (listToBag, emptyBag)
import GHC.Hs hiding (LHsTyVarBndr, HsTyVarBndr, HsModule)
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Types.Basic (SourceText(NoSourceText))
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import qualified GHC.Hs as GHC
#endif

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Construct @import qualified@ declaration
qimportD :: ModuleName -> LImportDecl GhcPs
qimportD name = noLoc $ ImportDecl {
      ideclExt       = defExt
    , ideclSourceSrc = NoSourceText
    , ideclName      = noLoc name
    , ideclPkgQual   = Nothing
    , ideclSafe      = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Nothing
#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = True
#else
    , ideclQualified = QualifiedPre
#endif
#if __GLASGOW_HASKELL__ < 900
    , ideclSource    = False
#else
    , ideclSource    = NotBoot
#endif
    }

conPat :: Located RdrName -> HsConPatDetails GhcPs -> Pat GhcPs
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#else
conPat x y = ConPat noExtField x y
#endif

funBind ::
     XFunBind idL idR
  -> Located (IdP idL)
  -> MatchGroup idR (LHsExpr idR)
  -> [Tickish Id]
  -> HsBindLR idL idR
#if __GLASGOW_HASKELL__ < 900
funBind ext id_ matches tick = FunBind {
      fun_ext     = ext
    , fun_id      = id_
    , fun_matches = matches
    , fun_tick    = tick
    , fun_co_fn   = WpHole -- TODO: Is WpHole here right..?
    }
#else
funBind = FunBind
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
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

{-------------------------------------------------------------------------------
  Generalized @forall@ in 9.0
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 900
type  HsTyVarBndr pass =  GHC.HsTyVarBndr () pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr () pass
#endif

hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
#if __GLASGOW_HASKELL__ < 900
hsFunTy = HsFunTy
#else
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#endif

userTyVar ::
     XUserTyVar pass
  -> Located (IdP pass)
  -> HsTyVarBndr pass
#if __GLASGOW_HASKELL__ < 900
userTyVar = UserTyVar
#else
userTyVar ext = UserTyVar ext ()
#endif

kindedTyVar ::
     XKindedTyVar pass
  -> Located (IdP pass)
  -> LHsKind pass
  -> HsTyVarBndr pass
#if __GLASGOW_HASKELL__ < 900
kindedTyVar = KindedTyVar
#else
kindedTyVar ext = KindedTyVar ext ()
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#else
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr Specificity pass
setDefaultSpecificity (L l v) = L l $ case v of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
    XTyVarBndr  ext              -> XTyVarBndr  ext
#endif






