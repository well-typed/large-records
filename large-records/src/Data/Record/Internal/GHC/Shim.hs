{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TupleSections          #-}
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

    -- * Locations
  , ToSrcSpan(..)
  , InheritLoc(..)
  , withoutLoc

    -- * New functionality
  , compareHs

    -- * NameCache
  , NameCacheIO
  , hscNameCacheIO
  , takeUniqFromNameCacheIO

    -- * Records
  , simpleRecordUpdates

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
  , module GHC.Utils.Error
#if __GLASGOW_HASKELL__ >= 902
  , module GHC.Types.SourceText
  , module GHC.Types.Fixity
#endif
#endif
  ) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Generics (Data, GenericQ, cast, toConstr, gzipWithQ)

import qualified Data.List.NonEmpty as NE

#if __GLASGOW_HASKELL__ < 900

import Data.IORef

import Bag (Bag, listToBag, emptyBag)
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
import GHC.Data.Bag (Bag, listToBag, emptyBag)
import GHC.Driver.Main (getHscEnv)
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Utils.Error (Severity(SevError, SevWarning))

import GHC.Plugins hiding ((<>), getHscEnv
#if __GLASGOW_HASKELL__ >= 902
    , AnnType, AnnLet, AnnRec, AnnLam, AnnCase
    , Exception
#endif
#if __GLASGOW_HASKELL__ < 904
    , trace
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

#if __GLASGOW_HASKELL__ < 904
import Data.IORef
import GHC.Iface.Env (lookupOrigIO)
import GHC.Types.Name.Cache (NameCache(nsUniqs))
#else
import GHC.Iface.Env (lookupNameCache)
import GHC.Rename.Names (renamePkgQual)
import GHC.Types.Name.Cache (NameCache, takeUniqFromNameCache)
#endif

#endif

{-------------------------------------------------------------------------------
  Name resolution
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

#if __GLASGOW_HASKELL__ >= 904
lookupOrigIO :: HscEnv -> Module -> OccName -> IO Name
lookupOrigIO env modl occ = lookupNameCache (hsc_NC env) modl occ
#endif

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: ModuleName -> Bool -> LImportDecl GhcPs
importDecl name qualified = noLocA $ ImportDecl {
      ideclExt       = defExt
    , ideclSourceSrc = NoSourceText
    , ideclName      = noLocA name
#if __GLASGOW_HASKELL__ >= 904
    , ideclPkgQual   = NoRawPkgQual
#else
    , ideclPkgQual   = Nothing
#endif
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

hsFunTy :: XFunTy GhcPs -> LHsType GhcPs -> LHsType GhcPs -> HsType GhcPs
#if __GLASGOW_HASKELL__ < 900
hsFunTy = HsFunTy
#elif __GLASGOW_HASKELL__ < 904
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#else
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow (L NoTokenLoc HsNormalTok))
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

class ToSrcSpan a where
  toSrcSpan :: a -> SrcSpan

instance ToSrcSpan SrcSpan where
  toSrcSpan = id

#if __GLASGOW_HASKELL__ >= 902
instance ToSrcSpan (SrcSpanAnn' a) where
  toSrcSpan = locA
#endif

instance ToSrcSpan l => ToSrcSpan (GenLocated l a) where
  toSrcSpan (L l _) = toSrcSpan l

instance ToSrcSpan a => ToSrcSpan (NonEmpty a) where
  toSrcSpan = toSrcSpan . NE.head

-- | The instance for @[]@ is not ideal: we use 'noLoc' if the list is empty
--
-- For the use cases in this library, this is acceptable: typically these are
-- lists with elements for the record fields, and having slightly poorer error
-- messages for highly unusual "empty large" records is fine.
instance ToSrcSpan a => ToSrcSpan [a] where
   toSrcSpan (a:_) = toSrcSpan a
   toSrcSpan []    = noSrcSpan

class InheritLoc x a b | b -> a where
  inheritLoc :: x -> a -> b

instance ToSrcSpan x => InheritLoc x a (GenLocated SrcSpan a) where
  inheritLoc = L . toSrcSpan

#if __GLASGOW_HASKELL__ >= 902
instance ToSrcSpan x => InheritLoc x a (GenLocated (SrcAnn ann) a) where
  inheritLoc = L . SrcSpanAnn defExt . toSrcSpan
#endif

instance InheritLoc x [a]                  [a]                  where inheritLoc _ = id
instance InheritLoc x Bool                 Bool                 where inheritLoc _ = id
instance InheritLoc x (HsTupArg p)         (HsTupArg p)         where inheritLoc _ = id
instance InheritLoc x (Pat p    )          (Pat p)              where inheritLoc _ = id
instance InheritLoc x (HsLocalBindsLR p q) (HsLocalBindsLR p q) where inheritLoc _ = id

withoutLoc :: InheritLoc SrcSpan a b => a -> b
withoutLoc = inheritLoc noSrcSpan

{-------------------------------------------------------------------------------
  Records
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 902
type RupdFlds = Either [LHsRecUpdField GhcPs] [LHsRecUpdProj GhcPs]
#else
type RupdFlds = [LHsRecUpdField GhcPs]
#endif

-- | Pattern match against the @rupd_flds@ of @RecordUpd@
simpleRecordUpdates :: RupdFlds -> Maybe [(LRdrName, LHsExpr GhcPs)]

#if __GLASGOW_HASKELL__ >= 904

simpleRecordUpdates =
    \case
      Left  flds -> mapM (aux (isUnambigous  . unLoc)) flds
      Right flds -> mapM (aux (isSingleLabel . unLoc)) flds
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe LRdrName)
      -> LHsFieldBind GhcPs lhs rhs
      -> Maybe (LRdrName, rhs)
    aux f (L _ (HsFieldBind { hfbLHS = lbl
                            , hfbRHS = val
                            , hfbPun = pun
                            })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe LRdrName
    isUnambigous (Unambiguous _ name) = Just $ reLoc name
    isUnambigous _                    = Nothing

    isSingleLabel :: FieldLabelStrings GhcPs -> Maybe LRdrName
    isSingleLabel (FieldLabelStrings labels) =
        case labels of
          [L _ (DotFieldOcc _ (L l label))] ->
            Just $ reLoc $ L l (Unqual $ mkVarOccFS label)
          _otherwise ->
            Nothing

#elif __GLASGOW_HASKELL__ == 902

simpleRecordUpdates =
    \case
      Left  flds -> mapM (aux isUnambigous)  flds
      Right flds -> mapM (aux isSingleLabel) flds
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe LRdrName)
      -> LHsRecField' GhcPs lhs rhs
      -> Maybe (LRdrName, rhs)
    aux f (L _ (HsRecField { hsRecFieldLbl = L _ lbl
                           , hsRecFieldArg = val
                           , hsRecPun      = pun
                           })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe LRdrName
    isUnambigous (Unambiguous _ name) = Just $ reLoc name
    isUnambigous _                    = Nothing

    isSingleLabel :: FieldLabelStrings GhcPs -> Maybe LRdrName
    isSingleLabel (FieldLabelStrings labels) =
        case labels of
          [L _ (HsFieldLabel _ (L l label))] ->
            Just $ L l (Unqual $ mkVarOccFS label)
          _otherwise ->
            Nothing

#else

simpleRecordUpdates =
     mapM (aux isUnambigous)
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe LRdrName)
      -> LHsRecField' lhs rhs
      -> Maybe (LRdrName, rhs)
    aux f (L _ (HsRecField { hsRecFieldLbl = L _ lbl
                           , hsRecFieldArg = val
                           , hsRecPun      = pun
                           })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe LRdrName
    isUnambigous (Unambiguous _ name) = Just $ reLoc name
    isUnambigous _                    = Nothing

#endif