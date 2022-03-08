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
--
-- TODO: Should the use of 'noLoc' be replaced with something better, so that
-- error messages point to the right place?
module Data.Record.Internal.GHC.TemplateHaskellStyle (
    -- * Names
    nameBase
  , mkExpVar
  , mkTyVar
  , mkTyCon
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
  , LRdrName
  ) where

import Data.List (foldl')

import Data.Record.Internal.GHC.Shim hiding (mkTyVar)

{-------------------------------------------------------------------------------
  Names
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Syntax.nameBase'
nameBase :: LRdrName -> String
nameBase = occNameString . rdrNameOcc . unLoc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for expression vars
mkExpVar :: SrcSpan -> String -> LRdrName
mkExpVar l = L l . mkRdrUnqual . mkVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type vars
mkTyVar :: SrcSpan -> String -> LRdrName
mkTyVar l = L l . mkRdrUnqual . mkTyVarOcc

-- | Equivalent of 'Language.Haskell.TH.Syntax.mkName', for type constructors
mkTyCon :: SrcSpan -> String -> LRdrName
mkTyCon l = L l . mkRdrUnqual . mkTcOcc

-- | Inverse to 'mkExpVar'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewExpVar :: LRdrName -> Maybe String
viewExpVar n | isVarOcc (rdrNameOcc (unLoc n)) = Just (nameBase n)
viewExpVar _otherwise = Nothing

-- | Inverse to 'mkTyVar'
--
-- NOTE: Defined in terms of 'nameBase', so discards qualifiers.
viewTyVar :: LRdrName -> Maybe String
viewTyVar n | isTvOcc (rdrNameOcc (unLoc n)) = Just (nameBase n)
viewTyVar _otherwise = Nothing

-- | Inverse to 'mkTyCon'
viewTyCon :: LRdrName -> Maybe String
viewTyCon n | isTcOcc (rdrNameOcc (unLoc n)) = Just (nameBase n)
viewTyCon _otherwise = Nothing

-- This patterns are not bidirectional: to construct a LRdrName, we need a
-- location. We may want to change this somehow. Use a Located String?

pattern ExpVar :: String -> LRdrName
pattern ExpVar n <- (viewExpVar -> Just n)

pattern TyVar :: String -> LRdrName
pattern TyVar n <- (viewTyVar -> Just n)

pattern TyCon :: String -> LRdrName
pattern TyCon n <- (viewTyCon -> Just n)

{-------------------------------------------------------------------------------
  Expressions
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varE'
--
-- TODO: We should assert that it's the right kind of name
varE :: LRdrName -> LHsExpr GhcPs
varE name = noLoc (HsVar defExt name)

-- | Inverse to 'varE'
viewVarE :: LHsExpr GhcPs -> Maybe LRdrName
viewVarE (L _ (HsVar _ name)) = Just name
viewVarE _                    = Nothing

pattern VarE :: LRdrName -> LHsExpr GhcPs
pattern VarE name <- (viewVarE -> Just name)
  where
    VarE = varE

-- | Equivalent of 'Language.Haskell.TH.Lib.conE'
--
-- TODO: We should assert that it's the right kind of name
conE :: LRdrName -> LHsExpr GhcPs
conE name = noLoc (HsVar defExt name)

-- | Inverse to 'conE'
viewConE :: LHsExpr GhcPs -> Maybe LRdrName
viewConE (L _ (HsVar _ name)) | isDataOcc (rdrNameOcc (unLoc name)) = Just name
viewConE _ = Nothing

pattern ConE :: LRdrName -> LHsExpr GhcPs
pattern ConE name <- (viewConE -> Just name)
  where
    ConE = conE

-- | Equivalent of 'Language.Haskell.TH.Lib.litE'
litE :: HsLit GhcPs -> LHsExpr GhcPs
litE = noLoc . HsLit defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.stringE'
stringE :: String -> LHsExpr GhcPs
stringE = litE . HsString NoSourceText . fsLit

-- | Equivalent of 'Language.Haskell.TH.Lib.recConE'
recConE :: LRdrName -> [(LRdrName, LHsExpr GhcPs)] -> LHsExpr GhcPs
recConE = \recName -> mkRec recName . map (uncurry mkFld)
  where
    mkRec :: LRdrName -> [LHsRecField GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
    mkRec name fields = noLoc $
        RecordCon defExt name (HsRecFields fields Nothing)

    mkFld :: LRdrName -> LHsExpr GhcPs -> LHsRecField GhcPs (LHsExpr GhcPs)
    mkFld name val = noLoc $
        HsRecField (noLoc (mkFieldOcc name)) val False

-- | Equivalent of 'Language.Haskell.TH.Lib.recUpdE'
recUpdE :: LHsExpr GhcPs -> [(LRdrName, LHsExpr GhcPs)] -> LHsExpr GhcPs
recUpdE = \recExpr -> updRec recExpr . map (uncurry updFld)
  where
    updRec :: LHsExpr GhcPs -> [LHsRecUpdField GhcPs] -> LHsExpr GhcPs
    updRec expr fields = noLoc $
        RecordUpd defExt expr fields

    updFld :: LRdrName -> LHsExpr GhcPs -> LHsRecUpdField GhcPs
    updFld name val = noLoc $
        HsRecField (noLoc (mkAmbiguousFieldOcc name)) val False

viewRecUpdE ::
     LHsExpr GhcPs
  -> Maybe (LHsExpr GhcPs, [(LRdrName, LHsExpr GhcPs)])
viewRecUpdE (L _ (RecordUpd _ recExpr fields)) =
    (recExpr,) <$> mapM viewFieldUpd fields
  where
    viewFieldUpd :: LHsRecUpdField GhcPs -> Maybe (LRdrName, LHsExpr GhcPs)
    viewFieldUpd (L _ (HsRecField (L _ (Unambiguous _ name)) val False)) =
        Just (name, val)
    viewFieldUpd _otherwise =
        Nothing
viewRecUpdE _otherwise = Nothing

pattern RecUpdE :: LHsExpr GhcPs -> [(LRdrName, LHsExpr GhcPs)] -> LHsExpr GhcPs
pattern RecUpdE recExpr fields <- (viewRecUpdE -> Just (recExpr, fields))
  where
    RecUpdE = recUpdE

-- | Equivalent of 'Language.Haskell.TH.Lib.appE'
appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = mkHsApp a b

-- | Equivalent of 'Language.Haskell.TH.Lib.listE'
listE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
listE = noLoc . ExplicitList defExt Nothing

-- | Equivalent of 'Language.Haskell.TH.Lib.lamE'
lamE :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE pats body = noLoc $
    HsLam defExt $
      MG defExt
         (noLoc [noLoc (Match defExt LambdaExpr pats (simpleGHRSs body))])
         Generated

-- | Equivalent of 'Language.Haskell.TH.Lib.caseE'
caseE :: LHsExpr GhcPs -> [(LPat GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
caseE x alts = noLoc $
    HsCase defExt x (MG defExt (noLoc (map mkAlt alts)) Generated)
  where
    mkAlt :: (LPat GhcPs, LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
    mkAlt (pat, body) = noLoc $
        Match defExt CaseAlt [pat] (simpleGHRSs body)

-- | Equivalent of 'Language.Haskell.TH.Lib.appsE'
appsE :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
appsE = foldl' appE

-- | Equivalent of 'Language.Haskell.TH.Lib.tupE'
tupE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
tupE xs = noLoc $
    ExplicitTuple defExt [noLoc (Present defExt x) | x <- xs] Boxed

-- | Equivalent of 'Language.Haskell.TH.Lib.sigE'
sigE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
sigE expr ty = noLoc $
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

-- | Equivalent of 'Language.Haskell.TH.Lib.litT'
litT :: HsTyLit -> LHsType GhcPs
litT = noLoc . HsTyLit defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.varT'
varT :: LRdrName -> LHsType GhcPs
varT name = noLoc (HsTyVar defExt NotPromoted name)

-- | Inverse to 'varT'
viewVarT :: LHsType GhcPs -> Maybe LRdrName
viewVarT (L _ (HsTyVar _ _ name)) = Just name
viewVarT _otherwise               = Nothing

pattern VarT :: LRdrName -> LHsType GhcPs
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
listT = noLoc . HsExplicitListTy defExt IsPromoted

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
funT a b = noLoc (hsFunTy defExt a b)

-- | Tuple type
--
-- TH only provides 'Language.Haskell.TH.Lib.tupleT'.
-- Signature by analogy with 'tupE'.
tupT :: [LHsType GhcPs] -> LHsType GhcPs
tupT = noLoc . HsExplicitTupleTy defExt

{-------------------------------------------------------------------------------
  Patterns
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.varP'
varP :: LRdrName -> LPat GhcPs
varP name = noLoc (VarPat defExt name)

-- | Equivalent of 'Language.Haskell.TH.Lib.conP'
conP :: LRdrName -> [LPat GhcPs] -> LPat GhcPs
conP con args = noLoc (conPat con (PrefixCon args))

-- | Equivalent of 'Language.Haskell.TH.Lib.bangP'
bangP :: LPat GhcPs -> LPat GhcPs
bangP = noLoc . BangPat defExt

-- | Equivalent of 'Language.Haskell.TH.Lib.listP'
listP :: [LPat GhcPs] -> LPat GhcPs
listP xs = noLoc (ListPat defExt xs)

-- | Equivalent of 'Language.Haskell.TH.Lib.wildP'
wildP :: LPat GhcPs
wildP = noLoc (WildPat defExt)

{-------------------------------------------------------------------------------
  Strictness
-------------------------------------------------------------------------------}

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.bangType'
--
-- The GHC API has no equivalent of 'Language.Haskell.TH.Syntax.BangType'.
bangType :: LHsType GhcPs -> LHsType GhcPs
bangType =
      noLoc
    . HsBangTy defExt (HsSrcBang NoSourceText NoSrcUnpack SrcStrict)

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
recC :: LRdrName -> [(LRdrName, LHsType GhcPs)] -> LConDecl GhcPs
recC = forallRecC [] []

-- | Inverse to 'recC'
viewRecC :: LConDecl GhcPs -> Maybe (LRdrName, [(LRdrName, LHsType GhcPs)])
viewRecC
    (L _
       ConDeclH98 {
           con_name   = conName
         , con_forall = L _ False
         , con_ex_tvs = []
         , con_mb_cxt = Nothing
         , con_args   = RecCon (L _ fields)
         }
    ) = (conName,) <$> mapM viewRecField fields
  where
    viewRecField :: LConDeclField GhcPs -> Maybe (LRdrName, LHsType GhcPs)
    viewRecField
        (L _
           ConDeclField {
               cd_fld_names = [L _ fieldName]
             , cd_fld_type  = ty
             }
        ) = Just $ (viewFieldOcc fieldName, ty)
    viewRecField _otherwise = Nothing

    viewFieldOcc :: FieldOcc GhcPs -> LRdrName
    viewFieldOcc (FieldOcc _ name) = name
#if __GLASGOW_HASKELL__ < 900
    viewFieldOcc _ = panic "viewFieldOcc"
#endif
viewRecC _otherwise = Nothing

pattern RecC :: LRdrName -> [(LRdrName, LHsType GhcPs)] -> LConDecl GhcPs
pattern RecC conName args <- (viewRecC -> Just (conName, args))
  where
    RecC = recC

-- | Equivalent of the combination of 'Language.Haskell.TH.Lib.forallC' and
-- 'Language.Haskell.TH.Lib.recC'.
forallRecC ::
     [LRdrName]                  -- ^ @forallC@ argument: bound type variables
  -> [LHsType GhcPs]             -- ^ @forallC@ argument: context
  -> LRdrName                    -- ^ @recC@ argument: record constructor name
  -> [(LRdrName, LHsType GhcPs)] -- ^ @recC@ argument: record fields
  -> LConDecl GhcPs
forallRecC vars ctxt conName args = noLoc $ ConDeclH98 {
      con_ext    = defExt
    , con_name   = conName
    , con_forall = noLoc True
    , con_ex_tvs = map (setDefaultSpecificity . noLoc . userTyVar defExt) vars
    , con_mb_cxt = Just (noLoc ctxt)
    , con_args   = RecCon (noLoc $ map (uncurry mkRecField) args)
    , con_doc    = Nothing
    }
  where
    mkRecField :: LRdrName -> LHsType GhcPs -> LConDeclField GhcPs
    mkRecField name ty = noLoc $ ConDeclField {
          cd_fld_ext   = defExt
        , cd_fld_names = [noLoc $ mkFieldOcc name]
        , cd_fld_type  = ty
        , cd_fld_doc   = Nothing
        }

{-------------------------------------------------------------------------------
  Type variable binders
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.kindedTV'
kindedTV :: LRdrName -> LHsType GhcPs -> LHsTyVarBndr GhcPs
kindedTV name ty = noLoc (kindedTyVar defExt name ty)

{-------------------------------------------------------------------------------
  .. without direct equivalent
-------------------------------------------------------------------------------}

tyVarBndrName :: LHsTyVarBndr GhcPs -> LRdrName
tyVarBndrName = hsTyVarLName . unLoc

{-------------------------------------------------------------------------------
  Top-level declarations
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.sigD'
sigD :: LRdrName -> LHsType GhcPs -> LHsDecl GhcPs
sigD name ty = noLoc $ SigD defExt sig
  where
    sig :: Sig GhcPs
    sig = TypeSig defExt [name] $ HsWC defExt (implicitBndrs ty)

-- | Equivalent of 'Language.Haskell.TH.Lib.valD'
--
-- Currently this offers a simplified API only.
valD :: LRdrName -> LHsExpr GhcPs -> LHsDecl GhcPs
valD fnName body = noLoc (ValD defExt (unLoc (simpleBinding fnName body)))

-- | Equivalent of 'Language.Haskell.TH.Lib.dataD'
dataD ::
     LRdrName                  -- ^ Datatype name
  -> [LHsTyVarBndr GhcPs]      -- ^ Type arguments
  -> [LConDecl GhcPs]          -- ^ Constructors
  -> [LHsDerivingClause GhcPs] -- ^ Deriving clauses
  -> LHsDecl GhcPs
dataD typeName tyVars cons derivs = noLoc $
    TyClD defExt $ DataDecl {
        tcdDExt     = defExt
      , tcdLName    = typeName
      , tcdTyVars   = mkHsQTvs tyVars
      , tcdFixity   = Prefix
      , tcdDataDefn = HsDataDefn {
            dd_ext     = defExt
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
         LRdrName
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
     LRdrName
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
    HsDerivingClause defExt strat (noLoc $ map implicitBndrs tys)

-- | Inverse of 'derivClause'
viewDerivClause ::
     LHsDerivingClause GhcPs
  -> (Maybe (LDerivStrategy GhcPs), [LHsType GhcPs])
viewDerivClause (L _ (HsDerivingClause _ mStrat (L _ tys))) =
    (mStrat, map viewImplicitBndrs tys)
#if __GLASGOW_HASKELL__ < 900
viewDerivClause _ = panic "viewDerivClause"
#endif

pattern DerivClause ::
     Maybe (LDerivStrategy GhcPs)
  -> [LHsType GhcPs]
  -> LHsDerivingClause GhcPs
pattern DerivClause strat tys <- (viewDerivClause -> (strat, tys))
  where
    DerivClause = derivClause

-- | Equivalent of 'Language.Haskell.TH.Lib.instanceD'
--
-- Unlike in TH, the regular bindings and associated types are separate args.
instanceD ::
     [LHsType GhcPs]            -- ^ Context
  -> LHsType GhcPs              -- ^ Head
  -> [(LRdrName, LHsExpr GhcPs)] -- ^ Bindings
  -> [LTyFamInstDecl GhcPs]     -- ^ Associated types
  -> LHsDecl GhcPs
instanceD ctxt hd binds assocTypes = noLoc $
    InstD defExt $ ClsInstD defExt $ ClsInstDecl {
        cid_ext           = defExt
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
    qualT ctx a = noLoc (HsQualTy defExt (noLoc ctx) a)

-- | Equivalent of 'Language.Haskell.TH.Lib.classD'
classD ::
     [LHsType GhcPs]            -- ^ Class context
  -> LRdrName                    -- ^ Class name
  -> [LHsTyVarBndr GhcPs]       -- ^ Type variables
  -> [(LRdrName, LHsType GhcPs)] -- ^ Method signatures
  -> LHsDecl GhcPs
classD = \ctx name clsVars sigs -> noLoc $
    TyClD defExt $ ClassDecl {
        tcdCExt   = defExt
      , tcdCtxt   = noLoc ctx
      , tcdLName  = name
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
    classOpSig :: LRdrName -> LHsType GhcPs -> LSig GhcPs
    classOpSig name ty = noLoc $
        ClassOpSig defExt False [name] (implicitBndrs ty)

-- | Approximate equivalent of 'Language.Haskell.TH.Lib.tySynEqn'
tySynEqn ::
     LRdrName         -- ^ Type family name
  -> [LHsType GhcPs] -- ^ Equation LHS
  -> LHsType GhcPs   -- ^ Equation RHS
  -> LTyFamInstDecl GhcPs
tySynEqn name pats val = noLoc $
    TyFamInstDecl $
      implicitBndrs $
        FamEqn defExt
               name
               Nothing
               (map HsValArg pats)
               Prefix
               val

{-------------------------------------------------------------------------------
  Pragmas

  NOTE: We work with 'LRdrName' everywhere, but 'AnnProvenance' /already/ wraps
  the @name@ type in @Located@.
-------------------------------------------------------------------------------}

-- | Equivalent of 'Language.Haskell.TH.Lib.typeAnnotation'
typeAnnotation :: LRdrName -> AnnProvenance RdrName
typeAnnotation name = TypeAnnProvenance name

-- | Inverse to 'typeAnnotation'
viewTypeAnnotation :: AnnProvenance RdrName -> Maybe LRdrName
viewTypeAnnotation (TypeAnnProvenance name) = Just name
viewTypeAnnotation _otherwise               = Nothing

pattern TypeAnnotation :: LRdrName -> AnnProvenance RdrName
pattern TypeAnnotation name <- (viewTypeAnnotation -> Just name)
  where
    TypeAnnotation = typeAnnotation

-- | Equivalent of 'Language.Haskell.TH.Lib.pragAnnD'
pragAnnD :: AnnProvenance RdrName -> LHsExpr GhcPs -> AnnDecl GhcPs
pragAnnD prov value =
    HsAnnotation
      defExt
      NoSourceText
      prov
      value

viewPragAnnD :: AnnDecl GhcPs -> (AnnProvenance RdrName, LHsExpr GhcPs)
viewPragAnnD (HsAnnotation _ _ prov value) = (prov, value)
#if __GLASGOW_HASKELL__ < 900
viewPragAnnD _ = panic "viewPragAnnD"
#endif

pattern PragAnnD :: AnnProvenance RdrName -> LHsExpr GhcPs -> AnnDecl GhcPs
pattern PragAnnD prov value <- (viewPragAnnD -> (prov, value))
  where
    PragAnnD = pragAnnD

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

implicitBndrs :: a -> HsImplicitBndrs GhcPs a
implicitBndrs a = HsIB defExt a

viewImplicitBndrs :: HsImplicitBndrs GhcPs a -> a
viewImplicitBndrs (HsIB _ a) = a
#if __GLASGOW_HASKELL__ < 900
viewImplicitBndrs _ = panic "viewImplicitBndrs"
#endif

-- | Simple binding (without patterns)
simpleBinding :: LRdrName -> LHsExpr GhcPs -> LHsBind GhcPs
simpleBinding fnName body = noLoc $
    funBind defExt fnName matchGroup []
  where
    grhs :: GRHSs GhcPs (LHsExpr GhcPs)
    grhs = simpleGHRSs body

    matchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
    matchGroup =
        MG defExt
           ( noLoc [ noLoc $ Match defExt
                                   (FunRhs fnName Prefix NoSrcStrict)
                                   []
                                   grhs
                   ]
           )
           Generated

-- | Simple guarded RHS (no guards)
simpleGHRSs :: LHsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
simpleGHRSs body =
    GRHSs defExt
          [noLoc $ GRHS defExt [] body]
          (noLoc $ EmptyLocalBinds defExt)


