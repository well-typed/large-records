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
    thNameToGhcNameHsc

    -- * Miscellaneous
  , importDecl
  , conPat
  , mkFunBind
  , HsModule
  , LHsModule
  , pattern GHC.HsModule

    -- * Annotations
#if __GLASGOW_HASKELL__ < 902
  , reLoc
  , noLocA
#endif
  , unLoc

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
  , FromSrcSpan(..)

    -- * New functionality
  , compareHs

    -- * NameCache
  , NameCacheIO
  , hscNameCacheIO
  , takeUniqFromNameCacheIO

    -- * Records
  , simpleRecordUpdates

    -- * Diagnostics
  , issueError
  , issueWarning

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
import GHC hiding (AnnKeywordId(..), HsModule, exprType, typeKind, mkFunBind, unLoc)
import GhcPlugins hiding ((<>), getHscEnv, unLoc)
import HscMain (getHscEnv)
import NameCache (NameCache(nsUniqs))
import PatSyn (PatSyn)
import TcEvidence (HsWrapper(WpHole))

import qualified GHC hiding (unLoc)
import qualified GhcPlugins as GHC hiding (unLoc)

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

import GHC.Plugins hiding ((<>), getHscEnv, unLoc
#if __GLASGOW_HASKELL__ >= 902
    , AnnType, AnnLet, AnnRec, AnnLam, AnnCase
    , Exception
#endif
#if __GLASGOW_HASKELL__ < 904
    , trace
#endif
#if __GLASGOW_HASKELL__ >= 908
    , fieldName
#endif
    )

#if __GLASGOW_HASKELL__ < 902
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Utils.Error (mkErrMsg, mkWarnMsg)
#else
import GHC.Types.Fixity
import GHC.Types.SourceText (SourceText(NoSourceText), mkIntegralLit)
#endif

#if __GLASGOW_HASKELL__ < 904
import Data.IORef
import GHC.Types.Name.Cache (NameCache(nsUniqs))
#else
import GHC.Types.Name.Cache (NameCache, takeUniqFromNameCache)
#endif

#endif

#if __GLASGOW_HASKELL__ >= 906
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
import qualified GHC.Types.Basic
#endif

#if __GLASGOW_HASKELL__ >= 902
import GHC.Utils.Logger (getLogger)
#endif

#if __GLASGOW_HASKELL__ == 902
import GHC.Types.Error (mkWarnMsg, mkErr, mkDecorated)
import GHC.Driver.Errors (printOrThrowWarnings)
#endif

#if __GLASGOW_HASKELL__ >= 904
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Errors (printOrThrowDiagnostics)
import GHC.Driver.Errors.Types (GhcMessage(GhcUnknownMessage))
import GHC.Types.Error (mkPlainError, mkMessages, mkPlainDiagnostic)
import GHC.Utils.Error (mkMsgEnvelope, mkErrorMsgEnvelope)
#endif

#if __GLASGOW_HASKELL__ >= 908
import GHC.Types.Error (mkSimpleUnknownDiagnostic)
#elif __GLASGOW_HASKELL__ >= 906
import GHC.Types.Error (UnknownDiagnostic(..))
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Driver.Config.Diagnostic (initPrintConfig)
#endif

import qualified Language.Haskell.TH as TH

-- thNameToGhcNameIO imports
#if !MIN_VERSION_ghc(9,0,0)
import Data.Maybe (listToMaybe)
import IfaceEnv (lookupOrigIO)
import GHC.ThToHs (thRdrNameGuesses)
import MonadUtils (mapMaybeM)
#elif !MIN_VERSION_ghc(9,4,0)
import Data.Maybe (listToMaybe)
import GHC.Iface.Env (lookupOrigIO)
import GHC.ThToHs (thRdrNameGuesses)
import GHC.Utils.Monad (mapMaybeM)
#endif

{-------------------------------------------------------------------------------
  Name resolution
-------------------------------------------------------------------------------}

thNameToGhcNameHsc :: TH.Name -> Hsc Name
thNameToGhcNameHsc th_name = do
  hsc_env <- getHscEnv
#if __GLASGOW_HASKELL__ >= 904
  let nameCache = hsc_NC hsc_env
#else
  let nameCache = hsc_env
#endif
  mname <- liftIO $ thNameToGhcNameIO nameCache th_name
  case mname of
    Just name -> return name
    Nothing -> issueError noSrcSpan $ text "Cannot lookup" <+> text (show th_name)

#if !MIN_VERSION_ghc(9,4,0)
thNameToGhcNameIO :: HscEnv -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO hscEnv th_name
  =  do { names <- mapMaybeM do_lookup (thRdrNameGuesses th_name)
        ; return (listToMaybe names) }
  where
    do_lookup rdr_name
      | Just n <- isExact_maybe rdr_name
      = return $ if isExternalName n then Just n else Nothing
      | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
#if MIN_VERSION_ghc(9,3,0)
      = Just <$> lookupNameCache (hsc_NC hscEnv) rdr_mod rdr_occ
#else
      = Just <$> lookupOrigIO hscEnv rdr_mod rdr_occ
#endif
      | otherwise
      = return Nothing
#endif

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: Bool -> ModuleName -> LImportDecl GhcPs
importDecl qualified name = noLocA $ ImportDecl {
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
    , ideclName      = noLocA name
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
    }

conPat :: LIdP GhcPs -> HsConPatDetails GhcPs -> Pat GhcPs
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#elif __GLASGOW_HASKELL__ < 910
conPat x y = ConPat defExt x y
#else
conPat x y = ConPat [] x y
#endif

mkFunBind :: LIdP GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)] -> HsBind GhcPs
#if __GLASGOW_HASKELL__ >= 908
mkFunBind n = GHC.mkFunBind defExt n
#else
mkFunBind n = GHC.mkFunBind Generated n
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
#endif

#if __GLASGOW_HASKELL__ >= 906
type LHsModule = Located (HsModule GhcPs)
#else
type LHsModule = Located HsModule
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
  Exact-print annotations
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ < 902
reLoc :: Located a -> Located a
reLoc = id

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

instance HasDefaultExt (Maybe a) where defExt = Nothing
instance HasDefaultExt [a] where defExt = []

instance HasDefaultExt NoExtField where
  defExt = noExtField

#if __GLASGOW_HASKELL__ >= 906
#if __GLASGOW_HASKELL__ < 910
instance HasDefaultExt (LayoutInfo GhcPs) where
  defExt = NoLayoutInfo
#endif
instance HasDefaultExt SourceText where
  defExt = NoSourceText
#elif __GLASGOW_HASKELL__ >= 900
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
#endif

#if __GLASGOW_HASKELL__ >= 910
instance HasDefaultExt GHC.Types.Basic.Origin where
  defExt = Generated OtherExpansion DoPmc
#elif __GLASGOW_HASKELL__ >= 908
instance HasDefaultExt GHC.Types.Basic.Origin where
  defExt = Generated DoPmc
#elif __GLASGOW_HASKELL__ >= 906
instance HasDefaultExt GHC.Types.Basic.Origin where
  defExt = Generated
#endif

instance (HasDefaultExt a, HasDefaultExt b) => HasDefaultExt (a, b) where
  defExt = (defExt, defExt)

instance (HasDefaultExt a, HasDefaultExt b, HasDefaultExt c) => HasDefaultExt (a, b, c) where
  defExt = (defExt, defExt, defExt)

#if __GLASGOW_HASKELL__ >= 910
instance HasDefaultExt (EpAnn ann) where
  defExt = error "silly GHC"

instance HasDefaultExt EpAnnComments where
  defExt = epAnnComments (noAnn @(EpAnn [()]))

#elif __GLASGOW_HASKELL__ >= 902
instance HasDefaultExt (EpAnn ann) where
  defExt = noAnn
instance HasDefaultExt AnnSortKey where
  defExt = NoAnnSortKey
instance HasDefaultExt EpAnnComments where
  defExt = epAnnComments noAnn
#endif

#if __GLASGOW_HASKELL__ >= 910
instance HasDefaultExt NoEpAnns         where defExt = NoEpAnns
instance HasDefaultExt AnnListItem      where defExt = noAnn
instance HasDefaultExt AnnPragma        where defExt = noAnn
instance HasDefaultExt AnnContext       where defExt = noAnn
instance HasDefaultExt AnnSig           where defExt = noAnn
instance HasDefaultExt AnnList          where defExt = noAnn
instance HasDefaultExt AnnParen         where defExt = noAnn
instance HasDefaultExt EpAnnHsCase      where defExt = noAnn
instance HasDefaultExt NameAnn          where defExt = noAnn
instance HasDefaultExt (AnnSortKey tag) where defExt = NoAnnSortKey

instance HasDefaultExt EpLayout         where defExt = EpNoLayout
instance HasDefaultExt (EpToken t)      where defExt = noAnn
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

#if __GLASGOW_HASKELL__ >= 908
type  HsTyVarBndr pass =  GHC.HsTyVarBndr (HsBndrVis GhcPs) pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr (HsBndrVis GhcPs) pass
#elif __GLASGOW_HASKELL__ >= 900
type  HsTyVarBndr pass =  GHC.HsTyVarBndr () pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr () pass
#endif

hsFunTy :: XFunTy GhcPs -> LHsType GhcPs -> LHsType GhcPs -> HsType GhcPs
#if __GLASGOW_HASKELL__ < 900
hsFunTy = HsFunTy
#elif __GLASGOW_HASKELL__ < 904
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#elif __GLASGOW_HASKELL__ < 910
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow (L NoTokenLoc HsNormalTok))
#else
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NoEpUniTok)
#endif

userTyVar ::
     XUserTyVar GhcPs
  -> LIdP GhcPs
  -> HsTyVarBndr GhcPs
#if __GLASGOW_HASKELL__ < 900
userTyVar = UserTyVar
#elif __GLASGOW_HASKELL__ < 908
userTyVar ext x = UserTyVar ext () x
#elif __GLASGOW_HASKELL__ < 910
userTyVar ext x = UserTyVar ext HsBndrRequired x
#else
userTyVar ext x = UserTyVar ext (HsBndrRequired defExt) x
#endif

kindedTyVar ::
     XKindedTyVar GhcPs
  -> LIdP GhcPs
  -> LHsKind GhcPs
  -> HsTyVarBndr GhcPs
#if __GLASGOW_HASKELL__ < 900
kindedTyVar = KindedTyVar
#elif __GLASGOW_HASKELL__ < 908
kindedTyVar ext k = KindedTyVar ext () k
#elif __GLASGOW_HASKELL__ < 910
kindedTyVar ext k = KindedTyVar ext HsBndrRequired k
#else
kindedTyVar ext k = KindedTyVar ext (HsBndrRequired defExt) k
#endif

-- | Like 'hsTyVarName', but don't throw away the location information
hsTyVarLName :: HsTyVarBndr GhcPs -> LIdP GhcPs
#if __GLASGOW_HASKELL__ < 900
hsTyVarLName (UserTyVar   _ n  ) = n
hsTyVarLName (KindedTyVar _ n _) = n
hsTyVarLName _ = panic "hsTyVarLName"
#else
hsTyVarLName (UserTyVar   _ _ n  ) = n
hsTyVarLName (KindedTyVar _ _ n _) = n
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#else
setDefaultSpecificity :: LHsTyVarBndr GhcPs -> GHC.LHsTyVarBndr Specificity GhcPs
setDefaultSpecificity = mapXRec @GhcPs $ \case
    UserTyVar   ext _ name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext _ name kind -> KindedTyVar ext SpecifiedSpec name kind
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
#if __GLASGOW_HASKELL__ < 910
instance ToSrcSpan (SrcSpanAnn' a) where
  toSrcSpan = locA
#endif
#endif

instance ToSrcSpan l => ToSrcSpan (GenLocated l a) where
  toSrcSpan (L l _) = toSrcSpan l

instance ToSrcSpan a => ToSrcSpan (NonEmpty a) where
  toSrcSpan = toSrcSpan . NE.head

#if __GLASGOW_HASKELL__ >= 910
instance ToSrcSpan (EpAnn ann) where
  toSrcSpan x = toSrcSpan (entry x)

instance ToSrcSpan EpaLocation where
  toSrcSpan = getHasLoc
#endif

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

class FromSrcSpan a where
    fromSrcSpan :: SrcSpan -> a

#if __GLASGOW_HASKELL__ >= 910
instance HasDefaultExt ann => FromSrcSpan (EpAnn ann) where
    fromSrcSpan s = EpAnn (spanAsAnchor s) defExt emptyComments
#endif

#if __GLASGOW_HASKELL__ >= 902
#if __GLASGOW_HASKELL__ >= 910
instance (ToSrcSpan x, FromSrcSpan y) => InheritLoc x a (GenLocated y a) where
  inheritLoc = L . fromSrcSpan . toSrcSpan
#else
instance ToSrcSpan x => InheritLoc x a (GenLocated (SrcAnn ann) a) where
  inheritLoc = L . SrcSpanAnn defExt . toSrcSpan
#endif
#endif

instance InheritLoc x [a]                  [a]                  where inheritLoc _ = id
instance InheritLoc x Bool                 Bool                 where inheritLoc _ = id
instance InheritLoc x (HsTupArg p)         (HsTupArg p)         where inheritLoc _ = id
instance InheritLoc x (Pat p    )          (Pat p)              where inheritLoc _ = id
instance InheritLoc x (HsLocalBindsLR p q) (HsLocalBindsLR p q) where inheritLoc _ = id

withoutLoc :: InheritLoc SrcSpan a b => a -> b
withoutLoc = inheritLoc noSrcSpan

-- GHC-8.10 has weird unLoc definition.
unLoc :: GenLocated l a -> a
unLoc (L _ a) = a

{-------------------------------------------------------------------------------
  Records
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 908
type RupdFlds = LHsRecUpdFields GhcPs
#elif __GLASGOW_HASKELL__ >= 902
type RupdFlds = Either [LHsRecUpdField GhcPs] [LHsRecUpdProj GhcPs]
#else
type RupdFlds = [LHsRecUpdField GhcPs]
#endif

-- | Pattern match against the @rupd_flds@ of @RecordUpd@
simpleRecordUpdates :: RupdFlds -> Maybe [(LIdP GhcPs, LHsExpr GhcPs)]

#if __GLASGOW_HASKELL__ >= 904

simpleRecordUpdates =
    \case
#if __GLASGOW_HASKELL__ >= 908
      RegularRecUpdFields _ flds ->
        mapM (aux (isUnambigous  . unLoc)) flds
      OverloadedRecUpdFields _ flds ->
        mapM (aux (isSingleLabel . unLoc)) flds
#else
      Left  flds -> mapM (aux (isUnambigous  . unLoc)) flds
      Right flds -> mapM (aux (isSingleLabel . unLoc)) flds
#endif
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe (LIdP GhcPs))
      -> LHsFieldBind GhcPs lhs rhs
      -> Maybe (LIdP GhcPs, rhs)
    aux f (L _ (HsFieldBind { hfbLHS = lbl
                            , hfbRHS = val
                            , hfbPun = pun
                            })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe (LIdP GhcPs)
    isUnambigous (Unambiguous _ name) = Just name
    isUnambigous _                    = Nothing

    isSingleLabel :: FieldLabelStrings GhcPs -> Maybe (LIdP GhcPs)
    isSingleLabel (FieldLabelStrings labels) =
        case labels of
#if __GLASGOW_HASKELL__ >= 906
          [L _ (DotFieldOcc _ (L l (FieldLabelString label)))] ->
#else
          [L _ (DotFieldOcc _ (L l label))] ->
#endif
            Just $ L l (Unqual $ mkVarOccFS label)
          _otherwise ->
            Nothing

#elif __GLASGOW_HASKELL__ == 902

simpleRecordUpdates =
    \case
      Left  flds -> mapM (aux isUnambigous)  flds
      Right flds -> mapM (aux isSingleLabel) flds
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe (LIdP GhcPs))
      -> LHsRecField' GhcPs lhs rhs
      -> Maybe (LIdP GhcPs, rhs)
    aux f (L _ (HsRecField { hsRecFieldLbl = L _ lbl
                           , hsRecFieldArg = val
                           , hsRecPun      = pun
                           })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe (LIdP GhcPs)
    isUnambigous (Unambiguous _ name) = Just name
    isUnambigous _                    = Nothing

    isSingleLabel :: FieldLabelStrings GhcPs -> Maybe (LIdP GhcPs)
    isSingleLabel (FieldLabelStrings labels) =
        case labels of
          [L _ (HsFieldLabel _ (L l label))] ->
            Just $ inheritLoc l (Unqual $ mkVarOccFS label)
          _otherwise ->
            Nothing

#else

simpleRecordUpdates =
     mapM (aux isUnambigous)
  where
    aux :: forall lhs rhs.
         (lhs -> Maybe (LIdP GhcPs))
      -> LHsRecField' lhs rhs
      -> Maybe (LIdP GhcPs, rhs)
    aux f (L _ (HsRecField { hsRecFieldLbl = L _ lbl
                           , hsRecFieldArg = val
                           , hsRecPun      = pun
                           })) = do
        guard $ not pun
        (, val) <$> f lbl

    isUnambigous :: AmbiguousFieldOcc GhcPs -> Maybe (LIdP GhcPs)
    isUnambigous (Unambiguous _ name) = Just $ reLoc name
    isUnambigous _                    = Nothing

#endif

{-------------------------------------------------------------------------------
  Diagnostics
-------------------------------------------------------------------------------}

issueError :: SrcSpan -> SDoc -> Hsc a
issueError l errMsg = do
#if __GLASGOW_HASKELL__ == 902
    throwOneError $
      mkErr l neverQualify (mkDecorated [errMsg])
#elif __GLASGOW_HASKELL__ >= 908
    throwOneError $
      mkErrorMsgEnvelope
        l
        neverQualify
        (GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError [] errMsg)
#elif __GLASGOW_HASKELL__ >= 906
    throwOneError $
      mkErrorMsgEnvelope
        l
        neverQualify
        (GhcUnknownMessage $ UnknownDiagnostic $ mkPlainError [] errMsg)
#elif __GLASGOW_HASKELL__ >= 904
    throwOneError $
      mkErrorMsgEnvelope
        l
        neverQualify
        (GhcUnknownMessage $ mkPlainError [] errMsg)
#else
    dynFlags <- getDynFlags
    throwOneError $
      mkErrMsg dynFlags l neverQualify errMsg
#endif

issueWarning :: SrcSpan -> SDoc -> Hsc ()
issueWarning l errMsg = do
    dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ == 902
    logger <- getLogger
    liftIO $ printOrThrowWarnings logger dynFlags . bag $
      mkWarnMsg l neverQualify errMsg
#elif __GLASGOW_HASKELL__ >= 906
    logger <- getLogger
    dflags <- getDynFlags
    let print_config = initPrintConfig dflags
    liftIO $ printOrThrowDiagnostics logger print_config (initDiagOpts dynFlags) . mkMessages . bag $
      mkMsgEnvelope
        (initDiagOpts dynFlags)
        l
        neverQualify
#if __GLASGOW_HASKELL__ >= 908
        (GhcUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainDiagnostic WarningWithoutFlag [] errMsg)
#else
        (GhcUnknownMessage $ UnknownDiagnostic $ mkPlainDiagnostic WarningWithoutFlag [] errMsg)
#endif
#elif __GLASGOW_HASKELL__ >= 904
    logger <- getLogger
    liftIO $ printOrThrowDiagnostics logger (initDiagOpts dynFlags) . mkMessages . bag $
      mkMsgEnvelope
        (initDiagOpts dynFlags)
        l
        neverQualify
        (GhcUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag [] errMsg)
#else
    liftIO $ printOrThrowWarnings dynFlags . bag $
      mkWarnMsg dynFlags l neverQualify errMsg
#endif
  where
    bag :: a -> Bag a
    bag = listToBag . (:[])

{-------------------------------------------------------------------------------
  Compat
-------------------------------------------------------------------------------}

-- pattern LambdaExpr = LamAlt LamSingle
