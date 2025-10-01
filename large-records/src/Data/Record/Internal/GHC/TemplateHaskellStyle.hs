{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Interface to the GHC API that closely mimicks Template Haskell
--
-- See "Language.Haskell.TH.Lib".
--
-- This module is intended to be CPP-free, with all CPP confined to
-- "Data.Record.Plugin.GHC.Shim". The only exception to this is the redundant
-- pattern matches that we need for the poor extension design in ghc 9.0; I've
-- not yet found a nice way to shim this.
module Data.Record.Internal.GHC.TemplateHaskellStyle (
    -- * Names
    nameBase
  , mkNameExp
  , mkNameTy
  , mkNameTyCon
  , pattern ExpVar
  , pattern TyVar
  , pattern TyCon
    -- * Expressions
  , litE
  , stringE
  , pattern VarE
  , pattern ConE
  , recConE
  , pattern RecUpdE
  , appE
  , listE
  , lamE
  , lamE1
  , caseE
  , appsE
  , appTypeE
  , tupE
  , sigE
    -- ** Without direct equivalent
  , intE
    -- * Types
  , parensT
  , litT
  , pattern VarT
  , pattern ConT
  , appT
  , listT
    -- ** Without direct equivalent
  , stringT
  , appsT
  , funT
  , tupT
    -- * Patterns
  , varP
  , conP
  , bangP
  , listP
  , wildP
    -- * Strictness
  , bangType
    -- * Class contexts
  , equalP
    -- * Constructors
  , pattern RecC
  , forallRecC
    -- * Type variable binders
  , kindedTV
    -- ** Without direct equivalent
  , tyVarBndrName
    -- * Top-level declarations
  , sigD
  , valD
  , pattern DataD
  , pattern DerivClause
  , instanceD
  , classD
  , tySynEqn
    -- * Pragmas
  , pattern TypeAnnotation
  , pattern PragAnnD

    -- * Re-exported types (intentionally without constructors)
    --
    -- We intentionally:
    --
    -- o Do not export constructors (unless otherwise indicated): the functions
    --   in this module are replacements for those constructors.
    -- o Only export the located versions of these types: we should try to
    --   minimize location loss when generating code, for better errors.
  , AnnDecl
  , DerivStrategy(..) -- Exported with constructors, is similar enough to TH
  , GhcPs
  , HsLit
  , HsTyLit
  , LConDecl
  , LDerivStrategy
  , LHsDecl
  , LHsDerivingClause
  , LHsExpr
  , LHsType
  , LHsTyVarBndr
  , LPat
  , LTyFamInstDecl
  ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NE

import Data.Record.Internal.GHC.Shim

{-------------------------------------------------------------------------------
  Internal auxiliary: types of names
-------------------------------------------------------------------------------}

isTermVar, isTermCon, isTypeVar, isTypeCon :: GenLocated l RdrName -> Bool
isTermVar = checkNameType isVarOcc
isTermCon = checkNameType isDataOcc
isTypeVar = checkNameType isTvOcc
isTypeCon = checkNameType isTcOcc


checkNameType :: (OccName -> Bool) -> GenLocated l RdrName -> Bool
checkNameType f (L _ n) = f (rdrNameOcc n)

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Syntax.nameBase'
nameBase :: GenLocated l RdrName -> String
nameBase = occNameString . rdrNameOcc . unLoc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for expression vars
mkNameExp :: SrcSpan -> String -> LIdP GhcPs
mkNameExp l = inheritLoc l . mkRdrUnqual . mkVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type vars
mkNameTy :: SrcSpan -> String -> LIdP GhcPs
mkNameTy l = inheritLoc l . mkRdrUnqual . mkTyVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type constructors
mkNameTyCon :: SrcSpan -> String -> LIdP GhcPs
mkNameTyCon l = inheritLoc l . mkRdrUnqual . mkTcOcc

-- | Inverse to 'mkNameExp'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewExpVar :: LIdP GhcPs -> Maybe String
viewExpVar n | isTermVar n = Just (nameBase n)
viewExpVar _otherwise = Nothing

-- | Inverse to 'mkNameTy'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewTyVar :: LIdP GhcPs -> Maybe String
viewTyVar n | isTypeVar n = Just (nameBase n)
viewTyVar _otherwise = Nothing

-- | Inverse to 'mkNameTyCon'
viewTyCon :: LIdP GhcPs -> Maybe String
viewTyCon n | isTypeCon n = Just (nameBase n)
viewTyCon _otherwise = Nothing

-- This patterns are not bidirectional: to construct a LIdP GhcPs, we need a
-- location. We may want to change this somehow. Use a Located String?

pattern ExpVar :: String -> LIdP GhcPs
pattern ExpVar n <- (viewExpVar -> Just n)

pattern TyVar :: String -> LIdP GhcPs
pattern TyVar n <- (viewTyVar -> Just n)

pattern TyCon :: String -> LIdP GhcPs
pattern TyCon n <- (viewTyCon -> Just n)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varE'
varE :: HasCallStack => LIdP GhcPs -> LHsExpr GhcPs
varE name
  | isTermVar name = inheritLoc name $ HsVar defExt name
  | otherwise      = error "varE: incorrect name type"

-- | Inverse to 'varE'
viewVarE :: LHsExpr GhcPs -> Maybe (LIdP GhcPs)
viewVarE (L _ (HsVar _ name)) | isTermVar name = Just name
viewVarE _ = Nothing

pattern VarE :: HasCallStack => () => LIdP GhcPs -> LHsExpr GhcPs
pattern VarE name <- (viewVarE -> Just name)
  where
    VarE = varE

-- | Equivalent of 'Language.Haskell.TH.Lib.conE'
conE :: HasCallStack => LIdP GhcPs -> LHsExpr GhcPs
conE name
  | isTermCon name = inheritLoc name $ HsVar defExt name
  | otherwise      = error "conE: incorrect name type"

-- | Inverse to 'conE'
viewConE :: LHsExpr GhcPs -> Maybe (LIdP GhcPs)
viewConE (L _ (HsVar _ name)) | isTermCon name = Just name
viewConE _ = Nothing

pattern ConE :: HasCallStack => () => LIdP GhcPs -> LHsExpr GhcPs
pattern ConE name <- (viewConE -> Just name)
  where
    ConE = conE

-- | Equivalent of 'Language.Haskell.TH.Lib.litE'
litE :: HsLit GhcPs -> LHsExpr GhcPs
litE = noLocA . HsLit defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.stringE'
stringE :: String -> LHsExpr GhcPs
stringE = litE . HsString NoSourceText . fsLit

-- | Equivalent of 'Language.Haskell.TH.Lib.recConE'
recConE :: LIdP GhcPs -> [(LIdP GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
recConE = \recName -> mkRec recName . map (uncurry mkFld)
  where
    mkRec :: LIdP GhcPs -> [LHsRecField GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
    mkRec name fields = inheritLoc name $
        RecordCon defExt name (HsRecFields
#if __GLASGOW_HASKELL__ >= 912
         defExt
#endif

         fields Nothing)

    mkFld :: LIdP GhcPs -> LHsExpr GhcPs -> LHsRecField GhcPs (LHsExpr GhcPs)
    mkFld name val = inheritLoc name $
#if __GLASGOW_HASKELL__ >= 904
        HsFieldBind defExt
#elif __GLASGOW_HASKELL__ >= 902
        HsRecField defExt
#else
        HsRecField
#endif
          (inheritLoc name (mkFieldOcc name)) val False

-- | Equivalent of 'Language.Haskell.TH.Lib.recUpdE'
recUpdE :: LHsExpr GhcPs -> [(LIdP GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
recUpdE = \recExpr -> updRec recExpr . map (uncurry updFld)
  where
#if __GLASGOW_HASKELL__ >= 908
    updRec :: LHsExpr GhcPs -> [LHsRecUpdField GhcPs GhcPs] -> LHsExpr GhcPs
#else
    updRec :: LHsExpr GhcPs -> [LHsRecUpdField GhcPs] -> LHsExpr GhcPs
#endif
    updRec expr fields = inheritLoc expr $
        RecordUpd defExt expr
#if __GLASGOW_HASKELL__ >= 908
          $ RegularRecUpdFields noExtField
#elif __GLASGOW_HASKELL__ >= 902
          $ Left
#endif
            fields

#if __GLASGOW_HASKELL__ >= 908
    updFld :: LIdP GhcPs -> LHsExpr GhcPs -> LHsRecUpdField GhcPs GhcPs
#else
    updFld :: LIdP GhcPs -> LHsExpr GhcPs -> LHsRecUpdField GhcPs
#endif
    updFld name val = inheritLoc name $
#if __GLASGOW_HASKELL__ >= 904
        HsFieldBind
#else
        HsRecField
#endif
#if __GLASGOW_HASKELL__ >= 902
          defExt
#endif
          (inheritLoc name (mkAmbiguousFieldOcc name)) val False

viewRecUpdE ::
     LHsExpr GhcPs
  -> Maybe (LHsExpr GhcPs, [(LIdP GhcPs, LHsExpr GhcPs)])
viewRecUpdE (L _ (RecordUpd _ recExpr fields)) =
    (recExpr,) <$> simpleRecordUpdates fields
viewRecUpdE _otherwise = Nothing

pattern RecUpdE :: LHsExpr GhcPs -> [(LIdP GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
pattern RecUpdE recExpr fields <- (viewRecUpdE -> Just (recExpr, fields))
  where
    RecUpdE = recUpdE

-- | Equivalent of 'Language.Haskell.TH.Lib.appE'
appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = mkHsApp a b

-- | Equivalent of 'Language.Haskell.TH.Lib.listE'
listE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
listE es = inheritLoc es $ ExplicitList defExt
#if __GLASGOW_HASKELL__ < 902
    Nothing
#endif
    es

-- | Equivalent of 'Language.Haskell.TH.Lib.lamE'
lamE :: NonEmpty (LPat GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE pats body = inheritLoc body $
    HsLam defExt
#if __GLASGOW_HASKELL__ >= 910
      LamSingle
#endif
      $
#if __GLASGOW_HASKELL__ >= 906
      MG defExt (inheritLoc body [inheritLoc body match])
#else
      MG defExt (inheritLoc body [inheritLoc body match]) Generated
#endif
  where
    match :: Match GhcPs (LHsExpr GhcPs)
    match = Match defExt LambdaExpr (inheritLoc pats (NE.toList pats)) (simpleGHRSs body)

-- | Convenience wrapper around 'lamE' for a single argument
lamE1 :: LPat GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE1 p = lamE (p :| [])

-- | Equivalent of 'Language.Haskell.TH.Lib.caseE'
caseE :: LHsExpr GhcPs -> [(LPat GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
caseE x alts = inheritLoc x $
#if __GLASGOW_HASKELL__ >= 906
    HsCase defExt x (MG defExt (inheritLoc x (map mkAlt alts)))
#else
    HsCase defExt x (MG defExt (inheritLoc x (map mkAlt alts)) Generated)
#endif
  where
    mkAlt :: (LPat GhcPs, LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
    mkAlt (pat, body) = inheritLoc x $
        Match defExt CaseAlt (inheritLoc pat [pat]) (simpleGHRSs body)

-- | Equivalent of 'Language.Haskell.TH.Lib.appsE'
appsE :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
appsE = Data.List.foldl' appE -- prefix to avoid unused import warning.

-- | Equivalent of 'Language.Haskell.TH.Lib.appT'
appTypeE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
appTypeE expr typ = inheritLoc expr $
#if __GLASGOW_HASKELL__ >= 910
    HsAppType defExt expr (HsWC defExt typ)
#elif __GLASGOW_HASKELL__ >= 906
    HsAppType noExtField expr noHsTok (HsWC defExt typ)
#else
    HsAppType
#if __GLASGOW_HASKELL__ >= 902
      (toSrcSpan expr)
#else
      defExt
#endif
      expr
      (HsWC defExt typ)
#endif
-- | Equivalent of 'Language.Haskell.TH.Lib.tupE'
tupE :: NonEmpty (LHsExpr GhcPs) -> LHsExpr GhcPs
tupE xs = inheritLoc xs $
    ExplicitTuple
      defExt
      [inheritLoc xs (Present defExt x) | x <- NE.toList xs]
      Boxed

-- | Equivalent of 'Language.Haskell.TH.Lib.sigE'
sigE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
sigE expr ty = inheritLoc expr $
    ExprWithTySig defExt expr (HsWC defExt (implicitBndrs ty))

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

-- | By analogy with 'stringE'
intE :: Integral a => a -> LHsExpr GhcPs
intE = litE . HsInt defExt . mkIntegralLit

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.parensT'
parensT :: LHsType GhcPs -> LHsType GhcPs
parensT = noLocA . HsParTy defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.litT'
#if __GLASGOW_HASKELL__ >= 906
litT :: HsTyLit GhcPs -> LHsType GhcPs
#else
litT :: HsTyLit -> LHsType GhcPs
#endif
litT = noLocA . HsTyLit defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.varT'
varT :: HasCallStack => LIdP GhcPs -> LHsType GhcPs
varT name
  | isTypeVar name = inheritLoc name (HsTyVar defExt NotPromoted name)
  | otherwise      = error "varT: incorrect name type"

-- | Inverse to 'varT'
viewVarT :: LHsType GhcPs -> Maybe (LIdP GhcPs)
viewVarT (L _ (HsTyVar _ _ name)) | isTypeVar name = Just name
viewVarT _otherwise = Nothing

pattern VarT :: HasCallStack => () => LIdP GhcPs -> LHsType GhcPs
pattern VarT name <- (viewVarT -> Just name)
  where
    VarT = varT

-- | Equivalent of 'Language.Haskell.TH.Lib.conT'
conT :: HasCallStack => LIdP GhcPs -> LHsType GhcPs
conT name
  | isTypeCon name = inheritLoc name (HsTyVar defExt NotPromoted name)
  | otherwise      = error "varT: incorrect name type"

-- | Inverse to 'conT'
viewConT :: LHsType GhcPs -> Maybe (LIdP GhcPs)
viewConT (L _ (HsTyVar _ _ name)) | isTypeCon name = Just name
viewConT _otherwise = Nothing

pattern ConT :: HasCallStack => () => LIdP GhcPs -> LHsType GhcPs
pattern ConT name <- (viewConT -> Just name)
  where
    ConT = conT

-- | Equivalent of 'Language.Haskell.TH.Lib.appT'
appT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
appT = mkHsAppTy

-- | Equivalent of 'Language.Haskell.TH.Lib.listT'
--
-- Signature by analogy with 'Language.Haskell.TH.Lib.listE'.
listT :: [LHsType GhcPs] -> LHsType GhcPs
listT ts = inheritLoc ts $ HsExplicitListTy defExt IsPromoted ts

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

-- | By analogy with 'stringE'
stringT :: String -> LHsType GhcPs
stringT = litT . HsStrTy NoSourceText . fsLit

-- | By analogy with 'appsE'
appsT :: LHsType GhcPs -> [LHsType GhcPs] -> LHsType GhcPs
appsT = foldl' appT

-- | Function type
--
-- TH only provides 'Language.Haskell.TH.Lib.arrowT'.
funT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
funT a b = inheritLoc a (hsFunTy defExt a b)

-- | Tuple type
--
-- TH only provides 'Language.Haskell.TH.Lib.tupleT'.
-- Signature by analogy with 'tupE'.
tupT :: NonEmpty (LHsType GhcPs) -> LHsType GhcPs
#if __GLASGOW_HASKELL__ >= 912
tupT ts = withoutLoc $ HsExplicitTupleTy defExt NotPromoted (NE.toList ts)
#else
tupT ts = inheritLoc ts $ HsExplicitTupleTy defExt (NE.toList ts)
#endif
{-------------------------------------------------------------------------------
  Patterns
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varP'
varP :: LIdP GhcPs -> LPat GhcPs
varP name = inheritLoc name (VarPat defExt name)

-- | Equivalent of 'Language.Haskell.TH.Lib.conP'
conP :: LIdP GhcPs -> [LPat GhcPs] -> LPat GhcPs
#if __GLASGOW_HASKELL__ >= 902
conP con args = inheritLoc con (conPat con (PrefixCon [] args))
#else
conP con args = inheritLoc con (conPat con (PrefixCon args))
#endif

-- | Equivalent of 'Language.Haskell.TH.Lib.bangP'
bangP :: LPat GhcPs -> LPat GhcPs
bangP p = inheritLoc p $ BangPat defExt p

-- | Equivalent of 'Language.Haskell.TH.Lib.listP'
listP :: [LPat GhcPs] -> LPat GhcPs
listP xs = inheritLoc xs $ ListPat defExt xs

-- | Equivalent of 'Language.Haskell.TH.Lib.wildP'
wildP :: LPat GhcPs
wildP = inheritLoc noSrcSpan (WildPat defExt)

{-------------------------------------------------------------------------------
  Strictness
-------------------------------------------------------------------------------}

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.bangType'
--
-- The GHC API has no equivalent of 'Language.Haskell.TH.Syntax.BangType'.
bangType :: LHsType GhcPs -> LHsType GhcPs
bangType t = inheritLoc t $
#if __GLASGOW_HASKELL__ >= 912
    HsBangTy defExt (HsBang NoSrcUnpack SrcStrict) t
#else
    HsBangTy defExt (HsSrcBang NoSourceText NoSrcUnpack SrcStrict) t
#endif
{-------------------------------------------------------------------------------
  Class contexts
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.equalP'
equalP :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
equalP x y = inheritLoc x $
    mkHsOpTy
#if __GLASGOW_HASKELL__ >= 904
      NotPromoted
#endif
      x
      (inheritLoc x eqTyCon_RDR)
      y

{-------------------------------------------------------------------------------
  Constructors
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.rec'
--
-- NOTE: The GHC AST (but not TH) supports declaring multiple record fields
-- with the same type. We do not support this here (since we follow TH).
recC :: LIdP GhcPs -> [(LIdP GhcPs, LHsType GhcPs, HsSrcBang)] -> LConDecl GhcPs
recC = forallRecC [] []

-- | Inverse to 'recC'
viewRecC :: LConDecl GhcPs -> Maybe (LIdP GhcPs, [(LIdP GhcPs, LHsType GhcPs, HsSrcBang)])
viewRecC
    (L _
       ConDeclH98 {
           con_name   = conName
#if __GLASGOW_HASKELL__ >= 902
         , con_forall = False
#else
         , con_forall = L _ False
#endif
         , con_ex_tvs = []
         , con_mb_cxt = Nothing
         , con_args   = RecCon (L _ fields)
         }
    ) = (conName ,) <$> mapM viewRecField fields
  where
    viewRecField :: LConDeclField GhcPs -> Maybe (LIdP GhcPs, LHsType GhcPs, HsSrcBang)
    viewRecField
        (L _
           ConDeclField {
               cd_fld_names = [L _ name]
             , cd_fld_type  = ty
             }
        ) = Just (viewFieldOcc name, getBangType ty, getBangStrictness ty)
    viewRecField _otherwise = Nothing

    viewFieldOcc :: FieldOcc GhcPs -> LIdP GhcPs
    viewFieldOcc (FieldOcc _ name) = name
#if __GLASGOW_HASKELL__ < 900
    viewFieldOcc _ = panic "viewFieldOcc"
#endif
viewRecC _otherwise = Nothing

pattern RecC :: LIdP GhcPs -> [(LIdP GhcPs, LHsType GhcPs, HsSrcBang)] -> LConDecl GhcPs
pattern RecC conName args <- (viewRecC -> Just (conName, args))
  where
    RecC = recC

-- | Equivalent of the combination of 'Language.Haskell.TH.Lib.forallC' and
-- 'Language.Haskell.TH.Lib.recC'.
forallRecC ::
     [LIdP GhcPs]                  -- ^ @forallC@ argument: bound type variables
  -> [LHsType GhcPs]               -- ^ @forallC@ argument: context
  -> LIdP GhcPs                    -- ^ @recC@ argument: record constructor name
  -> [(LIdP GhcPs, LHsType GhcPs, HsSrcBang)] -- ^ @recC@ argument: record fields
  -> LConDecl GhcPs
forallRecC vars ctxt conName args = inheritLoc conName $ ConDeclH98 {
      con_ext    = defExt
    , con_name   = conName
    , con_forall = inheritLoc conName True
    , con_ex_tvs = map (setDefaultSpecificity . mkBndr) vars
    , con_mb_cxt = Just (inheritLoc conName ctxt)
    , con_args   = RecCon (inheritLoc conName $ map mkRecField args)
    , con_doc    = Nothing
    }
  where
    mkBndr :: LIdP GhcPs -> LHsTyVarBndr GhcPs
    mkBndr name = inheritLoc name $ userTyVar name

    optionalBang :: HsSrcBang -> LHsType GhcPs -> LHsType GhcPs
    optionalBang bang = noLocA . HsBangTy defExt
#if __GLASGOW_HASKELL__ >= 912
      (case bang of HsSrcBang _ b -> b)
#else
      bang
#endif

    mkRecField :: (LIdP GhcPs, LHsType GhcPs, HsSrcBang) -> LConDeclField GhcPs
    mkRecField (name, ty, bang) = inheritLoc name $ ConDeclField {
          cd_fld_ext   = defExt
        , cd_fld_names = [inheritLoc name $ mkFieldOcc name]
        , cd_fld_type  = optionalBang bang ty
        , cd_fld_doc   = Nothing
        }

{-------------------------------------------------------------------------------
  Type variable binders
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.kindedTV'
kindedTV :: LIdP GhcPs -> LHsType GhcPs -> LHsTyVarBndr GhcPs
kindedTV name ty = inheritLoc name (kindedTyVar name ty)

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

tyVarBndrName :: LHsTyVarBndr GhcPs -> LIdP GhcPs
tyVarBndrName = fromMaybe (error "tyVarBndrName: Nothing") . hsTyVarLName . unLoc

{-------------------------------------------------------------------------------
  Top-level declarations
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.sigD'
sigD :: LIdP GhcPs -> LHsType GhcPs -> LHsDecl GhcPs
sigD name ty = inheritLoc name $ SigD defExt sig
  where
    sig :: Sig GhcPs
    sig = TypeSig defExt [name] $ HsWC defExt (implicitBndrs ty)

-- | Equivalent of 'Language.Haskell.TH.Lib.valD'
--
-- Currently this offers a simplified API only.
valD :: LIdP GhcPs -> LHsExpr GhcPs -> LHsDecl GhcPs
valD fnName body = inheritLoc fnName $
    ValD defExt (unLoc (simpleBinding fnName body))

-- | Equivalent of 'Language.Haskell.TH.Lib.dataD'
dataD ::
     LIdP GhcPs                -- ^ Datatype name
  -> [LHsTyVarBndr GhcPs]      -- ^ Type arguments
  -> [LConDecl GhcPs]          -- ^ Constructors
  -> [LHsDerivingClause GhcPs] -- ^ Deriving clauses
  -> LHsDecl GhcPs
dataD typeName tyVars cons derivs = inheritLoc typeName $
    TyClD defExt $ DataDecl {
        tcdDExt     = defExt
      , tcdLName    = typeName
      , tcdTyVars   = mkHsQTvs tyVars
      , tcdFixity   = Prefix
      , tcdDataDefn = HsDataDefn {
            dd_ext     = defExt
#if __GLASGOW_HASKELL__ >= 906
#else
          , dd_ND      = DataType
#endif
#if __GLASGOW_HASKELL__ >= 902
          , dd_ctxt    = Nothing
#else
          , dd_ctxt    = inheritLoc typeName []
#endif
          , dd_cType   = Nothing
          , dd_kindSig = Nothing
#if __GLASGOW_HASKELL__ >= 906
          , dd_cons    = DataTypeCons False cons
#else
          , dd_cons    = cons
#endif
          , dd_derivs  = inheritLoc typeName derivs
          }
      }

-- | Inverse to 'dataD'
viewDataD ::
     LHsDecl GhcPs
  -> Maybe (
         LIdP GhcPs
       , [LHsTyVarBndr GhcPs]
       , [LConDecl GhcPs]
       , [LHsDerivingClause GhcPs]
       )
viewDataD
    (L _
       (TyClD
         _
         DataDecl {
             tcdLName    = typeName
           , tcdTyVars   = HsQTvs {hsq_explicit = tyVars}
           , tcdFixity   = Prefix
           , tcdDataDefn = HsDataDefn {
#if __GLASGOW_HASKELL__ >= 902
                   dd_ctxt    = Nothing
#else
                   dd_ctxt    = L _ []
#endif
#if !__GLASGOW_HASKELL__ >= 906
                 , dd_ND      = DataType
#endif
                 , dd_cType   = Nothing
                 , dd_kindSig = Nothing
#if __GLASGOW_HASKELL__ >= 906
                 , dd_cons    = DataTypeCons False cons
#else
                 , dd_cons    = cons
#endif
#if __GLASGOW_HASKELL__ >= 902
                 , dd_derivs  = derivs
#else
                 , dd_derivs  = L _ derivs
#endif
                 }
           }
       )
    ) = Just (typeName, tyVars, cons, derivs)
viewDataD _otherwise = Nothing

pattern DataD ::
     LIdP GhcPs
  -> [LHsTyVarBndr GhcPs]
  -> [LConDecl GhcPs]
  -> [LHsDerivingClause GhcPs]
  -> LHsDecl GhcPs
pattern DataD typeName tyVars cons derivs <-
          (viewDataD -> Just (typeName, tyVars, cons, derivs))
  where
    DataD = dataD

-- | Equivalent of 'Language.Haskell.TH.derivClaus'
derivClause ::
     Maybe (LDerivStrategy GhcPs)
  -> NonEmpty (LHsType GhcPs)
  -> LHsDerivingClause GhcPs
derivClause strat tys = inheritLoc tys $
    HsDerivingClause defExt strat $ inheritLoc tys $
#if __GLASGOW_HASKELL__ >= 902
      DctMulti defExt $
#endif
      map implicitBndrs (NE.toList tys)

-- | Inverse of 'derivClause'
viewDerivClause ::
     LHsDerivingClause GhcPs
  -> (Maybe (LDerivStrategy GhcPs), [LHsType GhcPs])
#if __GLASGOW_HASKELL__ >= 902
viewDerivClause (L _ (HsDerivingClause _ mStrat (L _ (DctMulti _ tys)))) =
    (mStrat, map viewImplicitBndrs tys)
viewDerivClause (L _ (HsDerivingClause _ mStrat (L _ (DctSingle _ ty)))) =
    (mStrat, map viewImplicitBndrs [ty])
#else
viewDerivClause (L _ (HsDerivingClause _ mStrat (L _ tys))) =
    (mStrat, map viewImplicitBndrs tys)
#endif
#if __GLASGOW_HASKELL__ < 900
viewDerivClause _ = panic "viewDerivClause"
#endif

pattern DerivClause ::
     Maybe (LDerivStrategy GhcPs)
  -> NonEmpty (LHsType GhcPs)
  -> LHsDerivingClause GhcPs
pattern DerivClause strat tys <-
          (viewDerivClause -> (strat, NE.nonEmpty -> Just tys))
  where
    DerivClause = derivClause

-- | Equivalent of 'Language.Haskell.TH.Lib.instanceD'
--
-- Unlike in TH, the regular bindings and associated types are separate args.
instanceD ::
     [LHsType GhcPs]            -- ^ Context
  -> LHsType GhcPs              -- ^ Head
  -> [(LIdP GhcPs, LHsExpr GhcPs)] -- ^ Bindings
  -> [LTyFamInstDecl GhcPs]     -- ^ Associated types
  -> LHsDecl GhcPs
instanceD ctxt hd binds assocTypes = inheritLoc hd $
    InstD defExt $ ClsInstD defExt $ ClsInstDecl {
        cid_ext           = defExt
      , cid_poly_ty       = implicitBndrs (qualT ctxt hd)
#if __GLASGOW_HASKELL__ >= 912
      , cid_binds         = map (uncurry simpleBinding) binds
#else
      , cid_binds         = listToBag $ map (uncurry simpleBinding) binds
#endif
      , cid_sigs          = []
      , cid_tyfam_insts   = assocTypes
      , cid_datafam_insts = []
      , cid_overlap_mode  = Nothing
      }
  where
    qualT :: [LHsType GhcPs] -> LHsType GhcPs -> LHsType GhcPs
    qualT []        a = a
    qualT ctx@(c:_) a = inheritLoc c $
        HsQualTy
          defExt
#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 904
          (Just (inheritLoc c ctx))
#else
          (inheritLoc c ctx)
#endif
          a

-- | Equivalent of 'Language.Haskell.TH.Lib.classD'
classD ::
     [LHsType GhcPs]               -- ^ Class context
  -> LIdP GhcPs                    -- ^ Class name
  -> [LHsTyVarBndr GhcPs]          -- ^ Type variables
  -> [(LIdP GhcPs, LHsType GhcPs)] -- ^ Method signatures
  -> LHsDecl GhcPs
classD = \ctx name clsVars sigs -> inheritLoc name $
    TyClD defExt $ ClassDecl {
        tcdCExt   = defExt
#if __GLASGOW_HASKELL__ >= 906
#if __GLASGOW_HASKELL__ < 910
      , tcdLayout = NoLayoutInfo
#endif
#endif
#if __GLASGOW_HASKELL__ >= 902
      , tcdCtxt   = Just (inheritLoc name ctx)
#else
      , tcdCtxt   = inheritLoc name ctx
#endif
      , tcdLName  = name
      , tcdTyVars = mkHsQTvs clsVars
      , tcdFixity = Prefix
      , tcdFDs    = []
      , tcdSigs   = map (uncurry classOpSig) sigs
      , tcdMeths  = defExt
      , tcdATs    = []
      , tcdATDefs = []
      , tcdDocs   = []
      }
  where
    classOpSig :: LIdP GhcPs -> LHsType GhcPs -> LSig GhcPs
    classOpSig name ty = inheritLoc name $
        ClassOpSig defExt False [name] (implicitBndrs ty)

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.tySynEqn'
tySynEqn ::
     LIdP GhcPs         -- ^ Type family name
  -> [LHsType GhcPs] -- ^ Equation LHS
  -> LHsType GhcPs   -- ^ Equation RHS
  -> LTyFamInstDecl GhcPs
tySynEqn name pats val = inheritLoc val $
    TyFamInstDecl
#if __GLASGOW_HASKELL__ >= 902
      defExt $
#else
      $ implicitBndrs $
#endif
        FamEqn defExt
               name
#if __GLASGOW_HASKELL__ >= 902
               (HsOuterImplicit defExt)
#else
               Nothing
#endif
#if __GLASGOW_HASKELL__ >= 910
               (map (HsValArg defExt) pats)
#else
               (map HsValArg pats)
#endif
               Prefix
               val

{-------------------------------------------------------------------------------
  Pragmas

  NOTE: We work with 'LIdP GhcPs' everywhere, but 'AnnProvenance' /already/ wraps
  the @name@ type in @Located@.
-------------------------------------------------------------------------------}

type AnnProvenancePs = AnnProvenance
#if __GLASGOW_HASKELL__ >= 902
    GhcPs
#else
    RdrName
#endif

-- | Equivalent of 'Language.Haskell.TH.Lib.typeAnnotation'
typeAnnotation :: LIdP GhcPs -> AnnProvenancePs
typeAnnotation name = TypeAnnProvenance name

-- | Inverse to 'typeAnnotation'
viewTypeAnnotation :: AnnProvenancePs -> Maybe (LIdP GhcPs)
viewTypeAnnotation (TypeAnnProvenance name) = Just name
viewTypeAnnotation _otherwise               = Nothing

pattern TypeAnnotation :: LIdP GhcPs -> AnnProvenancePs
pattern TypeAnnotation name <- (viewTypeAnnotation -> Just name)
  where
    TypeAnnotation = typeAnnotation

-- | Equivalent of 'Language.Haskell.TH.Lib.pragAnnD'
pragAnnD :: AnnProvenancePs -> LHsExpr GhcPs -> AnnDecl GhcPs
pragAnnD prov value =
#if __GLASGOW_HASKELL__ >= 906
    HsAnnotation defExt prov value
#else
    HsAnnotation
      defExt
      NoSourceText
      prov
      value
#endif

viewPragAnnD :: AnnDecl GhcPs -> (AnnProvenancePs, LHsExpr GhcPs)
#if __GLASGOW_HASKELL__ >= 906
viewPragAnnD (HsAnnotation _ prov value) = (prov, value)
#else
viewPragAnnD (HsAnnotation _ _ prov value) = (prov, value)
#endif
#if __GLASGOW_HASKELL__ < 900
viewPragAnnD _ = panic "viewPragAnnD"
#endif

pattern PragAnnD :: AnnProvenancePs -> LHsExpr GhcPs -> AnnDecl GhcPs
pattern PragAnnD prov value <- (viewPragAnnD -> (prov, value))
  where
    PragAnnD = pragAnnD

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 902
implicitBndrs :: LHsType GhcPs -> LHsSigType GhcPs
implicitBndrs t = inheritLoc t (HsSig defExt (HsOuterImplicit defExt) t)

viewImplicitBndrs :: LHsSigType GhcPs -> LHsType GhcPs
viewImplicitBndrs (L _ (HsSig _ _ ty)) = ty

#else
implicitBndrs :: a -> HsImplicitBndrs GhcPs a
implicitBndrs a = HsIB defExt a

viewImplicitBndrs :: HsImplicitBndrs GhcPs a -> a
viewImplicitBndrs (HsIB _ a) = a
#if __GLASGOW_HASKELL__ < 900
viewImplicitBndrs _ = panic "viewImplicitBndrs"
#endif
#endif

-- | Simple binding (without patterns)
simpleBinding :: LIdP GhcPs -> LHsExpr GhcPs -> LHsBind GhcPs
simpleBinding fnName body = inheritLoc fnName $
    mkFunBind fnName [match]
  where
    grhs :: GRHSs GhcPs (LHsExpr GhcPs)
    grhs = simpleGHRSs body

    match :: LMatch GhcPs (LHsExpr GhcPs)
    match = inheritLoc fnName $
        Match defExt
#if __GLASGOW_HASKELL__ >= 912
              (FunRhs fnName Prefix NoSrcStrict defExt)
#else
              (FunRhs fnName Prefix NoSrcStrict)
#endif
#if __GLASGOW_HASKELL__ >= 912
              (withoutLoc [])
#else
              []
#endif
              grhs

-- | Simple guarded RHS (no guards)
simpleGHRSs :: LHsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
simpleGHRSs body =
    GRHSs defExt
          [inheritLoc body $ GRHS defExt [] body]
          (inheritLoc body $ EmptyLocalBinds defExt)
