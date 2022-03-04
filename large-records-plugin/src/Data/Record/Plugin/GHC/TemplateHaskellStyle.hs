{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Interface to the GHC API that closely mimicks Template Haskell
--
-- See "Language.Haskell.TH.Lib".
--
-- This module is intended to be CPP-free, with all CPP confined to
-- "Data.Record.Plugin.GHC.Shim".
--
-- TODO: Should the use of 'noLoc' be replaced with something better, so that
-- error messages point to the right place?
module Data.Record.Plugin.GHC.TemplateHaskellStyle (
    -- * Names
    nameBase
  , pattern ExpVar
  , pattern TyVar
  , pattern TyCon
    -- * Expressions
  , litE
  , stringE
  , pattern VarE
  , recConE
  , appE
  , listE
  , lamE
  , caseE
  , appsE
  , tupE
  , sigE
    -- ** Without direct equivalent
  , intE
    -- * Types
  , litT
  , pattern VarT
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
  , RdrName
  ) where

import Data.List (foldl')

import GHC
import GhcPlugins hiding (mkTyVar)
import Bag (listToBag, emptyBag)
import TcEvidence (HsWrapper(WpHole))

import Data.Record.Plugin.GHC.Shim (noExtField)

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Syntax.nameBase'
nameBase :: RdrName -> String
nameBase = occNameString . rdrNameOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for expression vars
mkExpVar :: String -> RdrName
mkExpVar = mkRdrUnqual . mkVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type vars
mkTyVar :: String -> RdrName
mkTyVar = mkRdrUnqual . mkTyVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type constructors
mkTyCon :: String -> RdrName
mkTyCon = mkRdrUnqual . mkTcOcc

-- | Inverse to 'mkExpVar'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewExpVar :: RdrName -> Maybe String
viewExpVar n | isVarOcc (rdrNameOcc n) = Just (nameBase n)
viewExpVar _otherwise = Nothing

-- | Inverse to 'mkTyVar'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewTyVar :: RdrName -> Maybe String
viewTyVar n | isTvOcc (rdrNameOcc n) = Just (nameBase n)
viewTyVar _otherwise = Nothing

-- | Inverse to 'mkTyCon'
viewTyCon :: RdrName -> Maybe String
viewTyCon n | isTcOcc (rdrNameOcc n) = Just (nameBase n)
viewTyCon _otherwise = Nothing

pattern ExpVar :: String -> RdrName
pattern ExpVar n <- (viewExpVar -> Just n)
  where
    ExpVar = mkExpVar

pattern TyVar :: String -> RdrName
pattern TyVar n <- (viewTyVar -> Just n)
  where
    TyVar = mkTyVar

pattern TyCon :: String -> RdrName
pattern TyCon n <- (viewTyCon -> Just n)
  where
    TyCon = mkTyCon

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varE'
varE :: RdrName -> LHsExpr GhcPs
varE name = noLoc (HsVar noExtField (noLoc name))

-- | Inverse to 'varE'
viewVarE :: LHsExpr GhcPs -> Maybe RdrName
viewVarE (L _ (HsVar _ (L _ name))) = Just name
viewVarE _                          = Nothing

pattern VarE :: RdrName -> LHsExpr GhcPs
pattern VarE name <- (viewVarE -> Just name)
  where
    VarE = varE

-- | Equivalent of 'Language.Haskell.TH.Lib.litE'
litE :: HsLit GhcPs -> LHsExpr GhcPs
litE = noLoc . HsLit noExtField

-- | Equivalent of 'Language.Haskell.TH.Lib.stringE'
stringE :: String -> LHsExpr GhcPs
stringE = litE . HsString NoSourceText . fsLit

-- | Equivalent of 'Language.Haskell.TH.Lib.recConE'
recConE :: RdrName -> [(RdrName, LHsExpr GhcPs)] -> LHsExpr GhcPs
recConE = \recName -> mkRec recName . map (uncurry mkFld)
  where
    mkRec :: RdrName -> [LHsRecField GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
    mkRec name fields = noLoc $
        RecordCon noExtField (noLoc name) (HsRecFields fields Nothing)

    mkFld :: RdrName -> LHsExpr GhcPs -> LHsRecField GhcPs (LHsExpr GhcPs)
    mkFld name val = noLoc $
        HsRecField (noLoc (mkFieldOcc (noLoc name))) val False

-- | Equivalent of 'Language.Haskell.TH.Lib.appE'
appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = mkHsApp a b

-- | Equivalent of 'Language.Haskell.TH.Lib.listE'
listE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
listE = noLoc . ExplicitList noExtField Nothing

-- | Equivalent of 'Language.Haskell.TH.Lib.lamE'
lamE :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE pats body = noLoc $
    HsLam noExtField $
      MG noExtField
         (noLoc [noLoc (Match noExtField LambdaExpr pats (simpleGHRSs body))])
         Generated

-- | Equivalent of 'Language.Haskell.TH.Lib.caseE'
caseE :: LHsExpr GhcPs -> [(LPat GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
caseE x alts = noLoc $
    HsCase noExtField x (MG noExtField (noLoc (map mkAlt alts)) Generated)
  where
    mkAlt :: (LPat GhcPs, LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
    mkAlt (pat, body) = noLoc $
        Match noExtField CaseAlt [pat] (simpleGHRSs body)

-- | Equivalent of 'Language.Haskell.TH.Lib.appsE'
appsE :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
appsE = foldl' appE

-- | Equivalent of 'Language.Haskell.TH.Lib.tupE'
tupE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
tupE xs = noLoc $
    ExplicitTuple noExtField [noLoc (Present noExtField x) | x <- xs] Boxed

-- | Equivalent of 'Language.Haskell.TH.Lib.sigE'
sigE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
sigE expr ty = noLoc $
    ExprWithTySig noExtField expr (HsWC noExtField (implicitBndrs ty))

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

-- | By analogy with 'stringE'
intE :: Integral a => a -> LHsExpr GhcPs
intE = litE . HsInt noExtField . mkIntegralLit

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.litT'
litT :: HsTyLit -> LHsType GhcPs
litT = noLoc . HsTyLit noExtField

-- | Equivalent of 'Language.Haskell.TH.Lib.varT'
varT :: RdrName -> LHsType GhcPs
varT name = noLoc (HsTyVar noExtField NotPromoted (noLoc name))

-- | Inverse to 'varT'
viewVarT :: LHsType GhcPs -> Maybe RdrName
viewVarT (L _ (HsTyVar _ _ (L _ name))) = Just name
viewVarT _otherwise                     = Nothing

pattern VarT :: RdrName -> LHsType GhcPs
pattern VarT name <- (viewVarT -> Just name)
  where
    VarT = varT

-- | Equivalent of 'Language.Haskell.TH.Lib.appT'
appT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
appT = mkHsAppTy

-- | Equivalent of 'Language.Haskell.TH.Lib.listT'
--
-- Signature by analogy with 'Language.Haskell.TH.Lib.listE'.
listT :: [LHsType GhcPs] -> LHsType GhcPs
listT = noLoc . HsExplicitListTy noExtField IsPromoted

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
funT a b = noLoc (HsFunTy noExtField a b)

-- | Tuple type
--
-- TH only provides 'Language.Haskell.TH.Lib.tupleT'.
-- Signature by analogy with 'tupE'.
tupT :: [LHsType GhcPs] -> LHsType GhcPs
tupT = noLoc . HsExplicitTupleTy noExtField

{-------------------------------------------------------------------------------
  Patterns
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varP'
varP :: RdrName -> LPat GhcPs
varP name = noLoc (VarPat noExtField (noLoc name))

-- | Equivalent of 'Language.Haskell.TH.Lib.conP'
conP :: RdrName -> [LPat GhcPs] -> LPat GhcPs
conP con args = noLoc (ConPatIn (noLoc con) (PrefixCon args))

-- | Equivalent of 'Language.Haskell.TH.Lib.bangP'
bangP :: LPat GhcPs -> LPat GhcPs
bangP = noLoc . BangPat noExtField

-- | Equivalent of 'Language.Haskell.TH.Lib.listP'
listP :: [LPat GhcPs] -> LPat GhcPs
listP xs = noLoc (ListPat noExtField xs)

-- | Equivalent of 'Language.Haskell.TH.Lib.wildP'
wildP :: LPat GhcPs
wildP = noLoc (WildPat noExtField)

{-------------------------------------------------------------------------------
  Strictness
-------------------------------------------------------------------------------}

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.bangType'
--
-- The GHC API has no equivalent of 'Language.Haskell.TH.Syntax.BangType'.
bangType :: LHsType GhcPs -> LHsType GhcPs
bangType =
      noLoc
    . HsBangTy noExtField (HsSrcBang NoSourceText NoSrcUnpack SrcStrict)

{-------------------------------------------------------------------------------
  Class contexts
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.equalP'
equalP :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
equalP x y = noLoc $ mkHsOpTy x (noLoc eqTyCon_RDR) y

{-------------------------------------------------------------------------------
  Constructors
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.rec'
--
-- NOTE: The GHC AST (but not TH) supports declaring multiple record fields
-- with the same type. We do not support this here (since we follow TH).
recC :: RdrName -> [(RdrName, LHsType GhcPs)] -> LConDecl GhcPs
recC = forallRecC [] []

-- | Inverse to 'recC'
viewRecC :: LConDecl GhcPs -> Maybe (RdrName, [(RdrName, LHsType GhcPs)])
viewRecC
    (L _
       ConDeclH98 {
           con_name = L _ conName
         , con_forall = L _ False
         , con_ex_tvs = []
         , con_mb_cxt = Nothing
         , con_args = RecCon (L _ fields)
         }
    ) = (conName,) <$> mapM viewRecField fields
  where
    viewRecField :: LConDeclField GhcPs -> Maybe (RdrName, LHsType GhcPs)
    viewRecField
        (L _
           ConDeclField {
               cd_fld_names = [L _ fieldName]
             , cd_fld_type  = ty
             }
        ) = (, ty) <$> viewFieldOcc fieldName
    viewRecField _otherwise = Nothing

    viewFieldOcc :: FieldOcc GhcPs -> Maybe RdrName
    viewFieldOcc (FieldOcc _ (L _ name)) = Just name
    viewFieldOcc _otherwise              = Nothing
viewRecC _otherwise = Nothing

pattern RecC :: RdrName -> [(RdrName, LHsType GhcPs)] -> LConDecl GhcPs
pattern RecC conName args <- (viewRecC -> Just (conName, args))
  where
    RecC = recC

-- | Equivalent of the combination of 'Language.Haskell.TH.Lib.forallC' and
-- 'Language.Haskell.TH.Lib.recC'.
forallRecC ::
     [RdrName]                  -- ^ @forallC@ argument: bound type variables
  -> [LHsType GhcPs]            -- ^ @forallC@ argument: context
  -> RdrName                    -- ^ @recC@ argument: record constructor name
  -> [(RdrName, LHsType GhcPs)] -- ^ @recC@ argument: record fields
  -> LConDecl GhcPs
forallRecC vars ctxt conName args = noLoc $ ConDeclH98 {
      con_ext    = noExtField
    , con_name   = noLoc conName
    , con_forall = noLoc True
    , con_ex_tvs = map (noLoc . UserTyVar noExtField . noLoc) vars
    , con_mb_cxt = Just (noLoc ctxt)
    , con_args   = RecCon (noLoc $ map (uncurry mkRecField) args)
    , con_doc    = Nothing
    }
  where
    mkRecField :: RdrName -> LHsType GhcPs -> LConDeclField GhcPs
    mkRecField name ty = noLoc $ ConDeclField {
          cd_fld_ext   = noExtField
        , cd_fld_names = [noLoc $ mkFieldOcc (noLoc name)]
        , cd_fld_type  = ty
        , cd_fld_doc   = Nothing
        }

{-------------------------------------------------------------------------------
  Type variable binders
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.kindedTV'
kindedTV :: RdrName -> LHsType GhcPs -> LHsTyVarBndr GhcPs
kindedTV name ty = noLoc (KindedTyVar noExtField (noLoc name) ty)

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

tyVarBndrName :: LHsTyVarBndr GhcPs -> RdrName
tyVarBndrName = hsTyVarName . unLoc

{-------------------------------------------------------------------------------
  Top-level declarations
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.sigD'
sigD :: RdrName -> LHsType GhcPs -> LHsDecl GhcPs
sigD name ty =  noLoc (SigD noExtField sig)
  where
    sig :: Sig GhcPs
    sig = TypeSig noExtField [noLoc name] $
            HsWC noExtField (implicitBndrs ty)

-- | Equivalent of 'Language.Haskell.TH.Lib.valD'
--
-- Currently this offers a simplified API only.
valD :: RdrName -> LHsExpr GhcPs -> LHsDecl GhcPs
valD fnName body = noLoc (ValD noExtField (unLoc (simpleBinding fnName body)))

-- | Equivalent of 'Language.Haskell.TH.Lib.dataD'
dataD ::
     RdrName                   -- ^ Datatype name
  -> [LHsTyVarBndr GhcPs]      -- ^ Type arguments
  -> [LConDecl GhcPs]          -- ^ Constructors
  -> [LHsDerivingClause GhcPs] -- ^ Deriving clauses
  -> LHsDecl GhcPs
dataD typeName tyVars cons derivs = noLoc $
    TyClD noExtField $ DataDecl {
        tcdDExt     = noExtField
      , tcdLName    = noLoc typeName
      , tcdTyVars   = mkHsQTvs tyVars
      , tcdFixity   = Prefix
      , tcdDataDefn = HsDataDefn {
            dd_ext     = noExtField
          , dd_ND      = DataType
          , dd_ctxt    = noLoc []
          , dd_cType   = Nothing
          , dd_kindSig = Nothing
          , dd_cons    = cons
          , dd_derivs  = noLoc derivs
          }
      }

-- | Inverse to 'dataD'
viewDataD ::
     LHsDecl GhcPs
  -> Maybe (
         RdrName
       , [LHsTyVarBndr GhcPs]
       , [LConDecl GhcPs]
       , [LHsDerivingClause GhcPs]
       )
viewDataD
    (L _
       (TyClD
         _
         DataDecl {
             tcdLName    = L _ typeName
           , tcdTyVars   = HsQTvs {hsq_explicit = tyVars}
           , tcdFixity   = Prefix
           , tcdDataDefn = HsDataDefn {
                   dd_ND      = DataType
                 , dd_ctxt    = L _ []
                 , dd_cType   = Nothing
                 , dd_kindSig = Nothing
                 , dd_cons    = cons
                 , dd_derivs  = L _ derivs
                 }
           }
       )
    ) = Just (typeName, tyVars, cons, derivs)
viewDataD _otherwise = Nothing

pattern DataD ::
     RdrName
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
  -> [LHsType GhcPs]
  -> LHsDerivingClause GhcPs
derivClause strat tys = noLoc $
    HsDerivingClause noExt strat (noLoc $ map implicitBndrs tys)

-- | Inverse of 'derivClause'
viewDerivClause ::
     LHsDerivingClause GhcPs
  -> Maybe (Maybe (LDerivStrategy GhcPs), [LHsType GhcPs])
viewDerivClause (L _ (HsDerivingClause _ mStrat (L _ tys))) =
    (mStrat,) <$> mapM viewImplicitBndrs tys
viewDerivClause _otherwise =
    Nothing

pattern DerivClause ::
     Maybe (LDerivStrategy GhcPs)
  -> [LHsType GhcPs]
  -> LHsDerivingClause GhcPs
pattern DerivClause strat tys <- (viewDerivClause -> Just (strat, tys))
  where
    DerivClause = derivClause

-- | Equivalent of 'Language.Haskell.TH.Lib.instanceD'
--
-- Unlike in TH, the regular bindings and associated types are separate args.
instanceD ::
     [LHsType GhcPs]            -- ^ Context
  -> LHsType GhcPs              -- ^ Head
  -> [(RdrName, LHsExpr GhcPs)] -- ^ Bindings
  -> [LTyFamInstDecl GhcPs]     -- ^ Associated types
  -> LHsDecl GhcPs
instanceD ctxt hd binds assocTypes = noLoc $
    InstD noExtField $ ClsInstD noExtField $ ClsInstDecl {
        cid_ext           = noExtField
      , cid_poly_ty       = implicitBndrs (qualT ctxt hd)
      , cid_binds         = listToBag $ map (uncurry simpleBinding) binds
      , cid_sigs          = []
      , cid_tyfam_insts   = assocTypes
      , cid_datafam_insts = []
      , cid_overlap_mode  = Nothing
      }
  where
    qualT :: [LHsType GhcPs] -> LHsType GhcPs -> LHsType GhcPs
    qualT []  a = a
    qualT ctx a = noLoc (HsQualTy noExtField (noLoc ctx) a)

-- | Equivalent of 'Language.Haskell.TH.Lib.classD'
classD ::
     [LHsType GhcPs]            -- ^ Class context
  -> RdrName                    -- ^ Class name
  -> [LHsTyVarBndr GhcPs]       -- ^ Type variables
  -> [(RdrName, LHsType GhcPs)] -- ^ Method signatures
  -> LHsDecl GhcPs
classD = \ctx name clsVars sigs -> noLoc $
    TyClD noExtField $ ClassDecl {
        tcdCExt   = noExtField
      , tcdCtxt   = noLoc ctx
      , tcdLName  = noLoc name
      , tcdTyVars = mkHsQTvs clsVars
      , tcdFixity = Prefix
      , tcdFDs    = []
      , tcdSigs   = map (uncurry classOpSig) sigs
      , tcdMeths  = emptyBag
      , tcdATs    = []
      , tcdATDefs = []
      , tcdDocs   = []
      }
  where
    classOpSig :: RdrName -> LHsType GhcPs -> LSig GhcPs
    classOpSig name ty = noLoc $
        ClassOpSig noExtField False [noLoc name] (implicitBndrs ty)

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.tySynEqn'
tySynEqn ::
     RdrName         -- ^ Type family name
  -> [LHsType GhcPs] -- ^ Equation LHS
  -> LHsType GhcPs   -- ^ Equation RHS
  -> LTyFamInstDecl GhcPs
tySynEqn name pats val = noLoc $
    TyFamInstDecl $
      implicitBndrs $
        FamEqn noExtField
               (noLoc name)
               Nothing
               (map HsValArg pats)
               Prefix
               val

{-------------------------------------------------------------------------------
  Pragmas
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.typeAnnotation'
typeAnnotation :: RdrName -> AnnProvenance RdrName
typeAnnotation name = TypeAnnProvenance (noLoc name)

-- | Inverse to 'typeAnnotation'
viewTypeAnnotation :: AnnProvenance RdrName -> Maybe RdrName
viewTypeAnnotation (TypeAnnProvenance (L _ name)) = Just name
viewTypeAnnotation _otherwise                     = Nothing

pattern TypeAnnotation :: RdrName -> AnnProvenance RdrName
pattern TypeAnnotation name <- (viewTypeAnnotation -> Just name)
  where
    TypeAnnotation = typeAnnotation

-- | Equivalent of 'Language.Haskell.TH.Lib.pragAnnD'
pragAnnD :: AnnProvenance RdrName -> LHsExpr GhcPs -> AnnDecl GhcPs
pragAnnD prov value =
    HsAnnotation
      noExt
      NoSourceText
      prov
      value

viewPragAnnD :: AnnDecl GhcPs -> Maybe (AnnProvenance RdrName, LHsExpr GhcPs)
viewPragAnnD (HsAnnotation _ _ prov value) = Just (prov, value)
viewPragAnnD _otherwise                    = Nothing

pattern PragAnnD :: AnnProvenance RdrName -> LHsExpr GhcPs -> AnnDecl GhcPs
pattern PragAnnD prov value <- (viewPragAnnD -> Just (prov, value))
  where
    PragAnnD = pragAnnD

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

implicitBndrs :: a -> HsImplicitBndrs GhcPs a
implicitBndrs a = HsIB noExt a

viewImplicitBndrs :: HsImplicitBndrs GhcPs a -> Maybe a
viewImplicitBndrs (HsIB _ a) = Just a
viewImplicitBndrs _otherwise = Nothing

-- | Simple binding (without patterns)
simpleBinding :: RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
simpleBinding fnName body = noLoc $
    -- TODO: Is WpHole here right..?
    FunBind noExtField (noLoc fnName) matchGroup WpHole []
  where
    grhs :: GRHSs GhcPs (LHsExpr GhcPs)
    grhs = simpleGHRSs body

    matchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
    matchGroup =
        MG noExtField
           ( noLoc [ noLoc $ Match noExtField
                                   (FunRhs (noLoc fnName) Prefix NoSrcStrict)
                                   []
                                   grhs
                   ]
           )
           Generated

-- | Simple guarded RHS (no guards)
simpleGHRSs :: LHsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
simpleGHRSs body =
    GRHSs noExtField
          [noLoc $ GRHS noExtField [] body]
          (noLoc $ EmptyLocalBinds noExtField)


