{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Internal.GHC.Shim (
    -- * Names
    lookupVarName
  , lookupTcName

    -- * Miscellaneous
  , importDecl
  , conPat
  , mkFunBind
  , HsModule
  , LHsModule
  , LRdrName
  , pattern GHC.HsModule

    -- * Annotations
#if __GLASGOW_HASKELL__ < 902
  , reLoc
  , reLocA
  , noLocA
#endif

    -- * Extensions
  , HasDefaultExt(..)
  , withDefExt

    -- * Generalized @forall@
#if __GLASGOW_HASKELL__ >= 900
  , HsTyVarBndr
  , LHsTyVarBndr
#endif
  , hsFunTy
  , userTyVar
  , kindedTyVar
  , hsTyVarLName
  , setDefaultSpecificity

    -- * New functionality
  , compareHs
  , inheritLoc
  , inheritLoc'
  , inheritLocPat

    -- * Re-exports

    -- The whole-sale module exports are not ideal for preserving compatibility
    -- across ghc versions, but we'll deal with this on a case by case basis.
#if __GLASGOW_HASKELL__ < 900
  , module Bag
  , module BasicTypes
  , module ErrUtils
  , module GHC
  , module GhcPlugins
  , module HscMain
  , module NameCache
  , module TcEvidence
#else
  , module GHC.Data.Bag
  , module GHC.Driver.Main
  , module GHC.Hs
  , module GHC.Plugins
  , module GHC.Tc.Types.Evidence
  , module GHC.Types.Name.Cache
  , module GHC.Utils.Error
#if __GLASGOW_HASKELL__ >= 902
  , module GHC.Types.SourceText
  , module GHC.Types.Fixity
#endif
#endif
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Generics (Data, GenericQ, cast, toConstr, gzipWithQ)

import qualified Data.List.NonEmpty as NE

#if __GLASGOW_HASKELL__ < 900

import Bag (listToBag, emptyBag)
import BasicTypes (SourceText (NoSourceText))
import ConLike (ConLike)
import ErrUtils (mkErrMsg, mkWarnMsg)
import Finder (findImportedModule)
import GHC hiding (AnnKeywordId(..), HsModule, exprType, typeKind, mkFunBind)
import GhcPlugins hiding ((<>), getHscEnv,)
import HscMain (getHscEnv)
import IfaceEnv (lookupOrigIO)
import NameCache (NameCache(nsUniqs))
import PatSyn (PatSyn)
import TcEvidence (HsWrapper(WpHole))

import qualified GHC
import qualified GhcPlugins as GHC

#else

import GHC.Hs hiding (LHsTyVarBndr, HsTyVarBndr, HsModule, mkFunBind)
import qualified GHC.Hs as GHC

import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.PatSyn (PatSyn)
import GHC.Data.Bag (listToBag, emptyBag)
import GHC.Driver.Main (getHscEnv)
import GHC.Iface.Env (lookupOrigIO)
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Types.Name.Cache (NameCache(nsUniqs))
import GHC.Utils.Error (Severity(SevError, SevWarning))

import GHC.Plugins hiding ((<>), getHscEnv
#if __GLASGOW_HASKELL__ >=902
    , AnnType, AnnLet, AnnRec, AnnLam, AnnCase
    , Exception
#endif
    )

#if __GLASGOW_HASKELL__ < 902
import GHC.Driver.Finder (findImportedModule)
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Utils.Error (mkErrMsg, mkWarnMsg)
#else
import GHC.Types.Fixity
import GHC.Types.SourceText (SourceText(NoSourceText), mkIntegralLit)
import GHC.Unit.Finder (findImportedModule, FindResult(Found))
#endif

#endif

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

lookupVarName ::
     HasCallStack
  => ModuleName
  -> Maybe FastString -- ^ Optional package name
  -> String -> Hsc Name
lookupVarName modl pkg = lookupOccName modl pkg . mkVarOcc

lookupTcName ::
     HasCallStack
  => ModuleName
  -> Maybe FastString -- ^ Optional package name
  -> String -> Hsc Name
lookupTcName modl pkg = lookupOccName modl pkg . mkTcOcc

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
      _otherwise   -> error $ concat [
          "lookupName: could not find "
        , occNameString name
        , " in module "
        , moduleNameString modlName
        , ". This might be due to an undeclared package dependency"
        , case mPkgName of
            Nothing  -> ""
            Just pkg -> " on " ++ unpackFS pkg
        , "."
        ]

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: ModuleName -> Bool -> LImportDecl GhcPs
importDecl name qualified = noLocA $ ImportDecl {
      ideclExt       = defExt
    , ideclSourceSrc = NoSourceText
    , ideclName      = noLocA name
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

conPat :: Located RdrName -> HsConPatDetails GhcPs -> Pat GhcPs
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#else
conPat x y = ConPat defExt (reLocA x) y
#endif

mkFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)] -> HsBind GhcPs
#if __GLASGOW_HASKELL__ < 810
mkFunBind = GHC.mkFunBind
#else
mkFunBind (reLocA -> n) = GHC.mkFunBind Generated n
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
#endif

type LHsModule = Located HsModule
type LRdrName  = Located RdrName

{-------------------------------------------------------------------------------
  Exact-print annotations
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ < 902
reLoc :: Located a -> Located a
reLoc = id

reLocA :: Located a -> Located a
reLocA = id

noLocA :: e -> Located e
noLocA = noLoc

#if __GLASGOW_HASKELL__ >= 900
mapXRec :: forall pass f g l. (f pass -> g pass) -> GenLocated l (f pass) -> GenLocated l (g pass)
mapXRec = fmap
#endif
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

instance (HasDefaultExt a, HasDefaultExt b) => HasDefaultExt (a, b) where
  defExt = (defExt, defExt)

instance (HasDefaultExt a, HasDefaultExt b, HasDefaultExt c) => HasDefaultExt (a, b, c) where
  defExt = (defExt, defExt, defExt)

#if __GLASGOW_HASKELL__ >= 902
instance HasDefaultExt (EpAnn ann) where
  defExt = noAnn

instance HasDefaultExt AnnSortKey where
  defExt = NoAnnSortKey

instance HasDefaultExt EpAnnComments where
  defExt = epAnnComments noAnn
#endif

-- In GHC-9.2 some things have extension fields.
#if __GLASGOW_HASKELL__ >= 902
withDefExt :: HasDefaultExt a => (a -> b) -> b
withDefExt f = f defExt
#else
withDefExt :: a -> a
withDefExt a = a
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
     XUserTyVar GhcPs
  -> Located (IdP GhcPs)
  -> HsTyVarBndr GhcPs
#if __GLASGOW_HASKELL__ < 900
userTyVar = UserTyVar
#else
userTyVar ext x = UserTyVar ext () (reLocA x)
#endif

kindedTyVar ::
     XKindedTyVar GhcPs
  -> Located (IdP GhcPs)
  -> LHsKind GhcPs
  -> HsTyVarBndr GhcPs
#if __GLASGOW_HASKELL__ < 900
kindedTyVar = KindedTyVar
#else
kindedTyVar ext k = KindedTyVar ext () (reLocA k)
#endif

-- | Like 'hsTyVarName', but don't throw away the location information
hsTyVarLName :: HsTyVarBndr GhcPs -> LRdrName
#if __GLASGOW_HASKELL__ < 900
hsTyVarLName (UserTyVar   _ n  ) = n
hsTyVarLName (KindedTyVar _ n _) = n
hsTyVarLName _ = panic "hsTyVarLName"
#else
hsTyVarLName (UserTyVar   _ _ n  ) = reLoc n
hsTyVarLName (KindedTyVar _ _ n _) = reLoc n
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#else
setDefaultSpecificity :: LHsTyVarBndr GhcPs -> GHC.LHsTyVarBndr Specificity GhcPs
setDefaultSpecificity = mapXRec @GhcPs $ \v -> case v of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
#if __GLASGOW_HASKELL__ < 900
    XTyVarBndr  ext              -> XTyVarBndr  ext
#endif
#endif

{-------------------------------------------------------------------------------
  New functionality
-------------------------------------------------------------------------------}

-- | Generic comparison for (parts of) the AST
--
-- NOTE: Not all abstract types are given special treatment here; in particular,
-- types only used in type-checked code ignored. To extend/audit this function,
-- grep the @ghc@ source for @abstractConstr@. Without further extensions,
-- all values of these types are considered equal.
--
-- NOTE: Although @ghc@ declares the constructor of @Bag@ as abstract as well,
-- we don't actually need a special case here: the constructors will be
-- considered equal, but 'gfoldl' will traverse the /elements/ of the @Bag@
-- nonetheless, which is precisely what we want.
compareHs' :: GenericQ (GenericQ Bool)
compareHs' x y
    | (Just x', Just y') <- (cast x, cast y) = (==) @ConLike     x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @PatSyn      x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @Class       x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @DataCon     x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @FastString  x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @Module      x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @ModuleName  x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @Name        x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @OccName     x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @TyCon       x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @UnitId      x' y'
    | (Just x', Just y') <- (cast x, cast y) = (==) @Var         x' y'
#if __GLASGOW_HASKELL__ >= 900
    | (Just x', Just y') <- (cast x, cast y) = (==) @Unit        x' y'
#endif
    | (Just x', Just y') <- (cast x, cast y) = ignr @RealSrcSpan x' y'
    | (Just x', Just y') <- (cast x, cast y) = ignr @SrcSpan     x' y'
    | otherwise = (toConstr x == toConstr y)
               && and (gzipWithQ compareHs' x y)
  where
    ignr :: a -> a -> Bool
    ignr _ _ = True

-- | Compare two (parts) of a Haskell source tree for equality
--
-- The trees are compared for literal equality, but 'SrcSpan's are ignored.
compareHs :: Data a => a -> a -> Bool
compareHs x y = compareHs' x y

{-------------------------------------------------------------------------------
  Working with locations
-------------------------------------------------------------------------------}

class FromSrcSpan l where
  fromSrcSpan :: SrcSpan -> l

instance FromSrcSpan SrcSpan where
  fromSrcSpan = id

#if __GLASGOW_HASKELL__ >= 902
instance HasDefaultExt ann => FromSrcSpan (SrcSpanAnn' ann) where
  fromSrcSpan = SrcSpanAnn defExt
#endif

class InheritLoc a where
  inheritLoc :: FromSrcSpan l => a -> b -> GenLocated l b

#if __GLASGOW_HASKELL__ >= 902
-- GHC-9.2 may not require annotation
inheritLoc' :: a -> b -> b
inheritLoc' _ = id
#else
inheritLoc' :: InheritLoc a => a -> b -> Located b
inheritLoc' = inheritLoc
#endif

instance InheritLoc a => InheritLoc (NonEmpty a) where
  inheritLoc = inheritLoc . NE.head

instance InheritLoc l => InheritLoc (GenLocated l a) where
  inheritLoc (L l _) = inheritLoc l

instance InheritLoc SrcSpan where
  inheritLoc l x = L (fromSrcSpan l) x

#if __GLASGOW_HASKELL__ >= 902
instance InheritLoc (SrcSpanAnn' ann) where
  inheritLoc (SrcSpanAnn _ l) = inheritLoc l
#endif

--
-- -- | The instance for @[]@ is not ideal: we use 'noLoc' if the list is empty
-- --
-- -- For the use cases in this library, this is acceptable: typically these are
-- -- lists with elements for the record fields, and having slightly poorer error
-- -- messages for highly unusual "empty large" records is fine.
instance InheritLoc a => InheritLoc [a] where
   inheritLoc (a:_) = inheritLoc a
   inheritLoc []    = inheritLoc noSrcSpan

#if __GLASGOW_HASKELL__ < 810
inheritLocPat :: a -> Pat p -> LPat p
inheritLocPat _ = id -- In 8.8, 'LPat' is a synonym for 'Pat'
#else
inheritLocPat :: InheritLoc a => a -> Pat (GhcPass p) -> LPat (GhcPass p)
inheritLocPat = inheritLoc
#endif
