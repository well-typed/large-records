{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | GHC AST utilities and reexports.
module Data.Record.Plugin.GHC
  ( module Data.Record.Plugin.GHC,
    module OccName,
    module RdrName,
    module GHC,
    module BasicTypes,
    module GhcPlugins,
    module Outputable,
    module DynFlags,
  )
where

import Bag (emptyBag, listToBag)
import BasicTypes
import Data.List (foldl')
import Data.String (fromString)
import DynFlags (getDynFlags)
import GHC
import GhcPlugins (HsParsedModule (..), Plugin (..), defaultPlugin, purePlugin)
import OccName
import Outputable (ppr, showSDoc)
import RdrName
import TcEvidence (HsWrapper (WpHole))
import TysWiredIn (eqTyCon_RDR)

#if __GLASGOW_HASKELL__ < 810
noExtField :: NoExt
noExtField = noExt
#endif

-- | Get name from possibly qualified name.
-- - @A.b@ -> @"b"@
-- - @b@ -> @"b"@
--
-- Note: module name will be ignored
rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

-- | Makes type variable from string
varRdrT :: String -> RdrName
varRdrT = mkRdrUnqual . mkTyVarOcc

-- | Makes variable from string
varRdr :: String -> RdrName
varRdr = mkRdrUnqual . mkVarOcc

litE :: HsLit GhcPs -> LHsExpr GhcPs
litE = noLoc . HsLit noExtField

litT :: HsTyLit -> LHsType GhcPs
litT = noLoc . HsTyLit noExtField

stringE :: String -> LHsExpr GhcPs
stringE = litE . HsString NoSourceText . fromString

stringT :: String -> LHsType GhcPs
stringT = litT . HsStrTy NoSourceText . fromString

intE :: Integral a => a -> LHsExpr GhcPs
intE = litE . HsInt noExtField . mkIntegralLit

varT :: RdrName -> LHsType GhcPs
varT name = noLoc (HsTyVar noExtField NotPromoted (noLoc name))

varE :: RdrName -> LHsExpr GhcPs
varE name = noLoc (HsVar noExtField (noLoc name))

varP :: RdrName -> LPat GhcPs
varP name = noLoc (VarPat noExtField (noLoc name))

conP :: RdrName -> [LPat GhcPs] -> LPat GhcPs
conP con args = noLoc (ConPatIn (noLoc con) (PrefixCon args))

opT :: LHsType GhcPs -> RdrName -> LHsType GhcPs -> LHsType GhcPs
opT l op r = noLoc (mkHsOpTy l (noLoc op) r)

bangT :: LHsType GhcPs -> LHsType GhcPs
bangT = noLoc . HsBangTy noExtField (HsSrcBang NoSourceText NoSrcUnpack SrcStrict)

-- | @qualT [a, b] c@ -> @([a], [b]) => [c]@
qualT :: [LHsType GhcPs] -> LHsType GhcPs -> LHsType GhcPs
qualT ctx a = noLoc (HsQualTy noExtField (noLoc ctx) a)

arrT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
arrT a b = noLoc (HsFunTy noExtField a b)

appT :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
appT = mkHsAppTy

parE :: LHsExpr GhcPs -> LHsExpr GhcPs
parE = noLoc . HsPar noExtField

recFieldE :: RdrName -> LHsExpr GhcPs -> LHsRecField GhcPs (LHsExpr GhcPs)
recFieldE name val = noLoc (HsRecField (noLoc (mkFieldOcc (noLoc name))) val False)

recConE :: RdrName -> [LHsRecField GhcPs (LHsExpr GhcPs)] -> LHsExpr GhcPs
recConE name fields = noLoc (RecordCon noExtField (noLoc name) (HsRecFields fields Nothing))

bangP :: LPat GhcPs -> LPat GhcPs
bangP = noLoc . BangPat noExtField

appE :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
appE a b = mkHsApp a b

listE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
listE = noLoc . ExplicitList noExtField Nothing

listT :: [LHsType GhcPs] -> LHsType GhcPs
listT = noLoc . HsExplicitListTy noExtField IsPromoted

tupleE :: [LHsExpr GhcPs] -> LHsExpr GhcPs
tupleE xs = noLoc (ExplicitTuple noExtField [noLoc (Present noExtField x) | x <- xs] Boxed)

tupleT :: [LHsType GhcPs] -> LHsType GhcPs
tupleT = noLoc . HsExplicitTupleTy noExtField

listP :: [LPat GhcPs] -> LPat GhcPs
listP xs = noLoc (ListPat noExtField xs)

appsT :: LHsType GhcPs -> [LHsType GhcPs] -> LHsType GhcPs
appsT = foldl' appT

appsE :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
appsE = foldl' appE

wildP :: LPat GhcPs
wildP = noLoc (WildPat noExtField)

kindedTyVarBndr :: RdrName -> LHsType GhcPs -> LHsTyVarBndr GhcPs
kindedTyVarBndr name ty = noLoc (KindedTyVar noExtField (noLoc name) ty)

caseE :: LHsExpr GhcPs -> [(LPat GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs
caseE x alts =
  noLoc (HsCase noExtField x (MG noExtField (noLoc (map mkAlt alts)) Generated))
  where
    mkAlt :: (LPat GhcPs, LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs)
    mkAlt (pat, body) = noLoc (Match noExtField CaseAlt [pat] (simpleGHRSs body))

conDeclField :: RdrName -> LHsType GhcPs -> LConDeclField GhcPs
conDeclField name ty =
  noLoc
    ConDeclField
      { cd_fld_ext = noExtField,
        cd_fld_names = [noLoc (mkFieldOcc (noLoc name))],
        cd_fld_type = ty,
        cd_fld_doc = Nothing
      }

-- | A helper for constructing 'GRHSs' (guarded right-hand sides) in the simple guard-less case.
simpleGHRSs :: LHsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
simpleGHRSs body = GRHSs noExtField [noLoc (GRHS noExtField [] body)] (noLoc (EmptyLocalBinds noExtField))

lamE :: [LPat GhcPs] -> LHsExpr GhcPs -> LHsExpr GhcPs
lamE pats body = noLoc (HsLam noExtField (MG noExtField (noLoc [noLoc (Match noExtField LambdaExpr pats (simpleGHRSs body))]) Generated))

typeSigE :: LHsExpr GhcPs -> LHsType GhcPs -> LHsExpr GhcPs
typeSigE expr ty = noLoc (ExprWithTySig noExtField expr (HsWC noExtField (HsIB noExtField ty)))

typeSig :: RdrName -> LHsType GhcPs -> LSig GhcPs
typeSig name ty = noLoc (TypeSig noExtField [noLoc name] (HsWC noExtField (HsIB noExtField ty)))

classOpSig :: RdrName -> LHsType GhcPs -> LSig GhcPs
classOpSig name ty = noLoc (ClassOpSig noExtField False [noLoc name] (HsIB noExtField ty))

-- | A helper for constructing @type instance@ AST.
tfInstanceD ::
  -- | Type family name
  RdrName ->
  -- | Equation LHS
  [LHsType GhcPs] ->
  -- | Equation RHS
  LHsType GhcPs ->
  LTyFamInstDecl GhcPs
tfInstanceD name pats val = noLoc (TyFamInstDecl (HsIB noExtField (FamEqn noExtField (noLoc name) Nothing [HsValArg p | p <- pats] Prefix val)))

-- | A helper for constructing simple function definitions with one equation and without patterns.
simpleFn ::
  -- | Function name
  RdrName ->
  -- | Function signature
  LHsType GhcPs ->
  -- | Equation RHS
  LHsExpr GhcPs ->
  [LHsDecl GhcPs]
simpleFn fnName ty body =
  [ noLoc (SigD noExtField (unLoc (typeSig fnName ty))),
    noLoc (ValD noExtField (unLoc (simpleBind fnName body)))
  ]

anyclassDeriving :: LHsType GhcPs -> LHsDerivingClause GhcPs
anyclassDeriving c = noLoc (HsDerivingClause noExtField (Just (noLoc AnyclassStrategy)) (noLoc [HsIB noExtField c]))

typeEq :: RdrName
typeEq = eqTyCon_RDR

-- | A helper for constructing simple bindings without patterns.
simpleBind ::
  -- | Function/variable name
  RdrName ->
  -- | RHS
  LHsExpr GhcPs ->
  LHsBind GhcPs
simpleBind fnName body =
  let body' = simpleGHRSs body
      body'' = MG noExtField (noLoc [noLoc (Match noExtField (FunRhs (noLoc fnName) Prefix NoSrcStrict) [] body')]) Generated
   in noLoc (FunBind noExtField (noLoc fnName) body'' WpHole [])

-- | A helper for constructing @instance@ AST, mimicking Template Haskell.
instanceD_simple :: 
  -- | Instance context
  [LHsType GhcPs] ->
  -- | Instance head
  LHsType GhcPs ->
  -- | Bindings
  [LHsBind GhcPs] ->
  -- | Associated types
  [LTyFamInstDecl GhcPs] ->
  LHsDecl GhcPs
instanceD_simple ctx head binds assocTypes = noLoc (InstD noExtField (ClsInstD noExtField inst))
  where
    inst =
      ClsInstDecl
        { cid_ext = noExtField,
          cid_poly_ty = HsIB noExtField (if null ctx then head else qualT ctx head),
          cid_binds = listToBag binds,
          cid_sigs = [],
          cid_tyfam_insts = assocTypes,
          cid_datafam_insts = [],
          cid_overlap_mode = Nothing
        }

-- | A helper for constructing @class@ AST, mimicking Template Haskell.
classD_simple ::
  -- | Class context
  [LHsType GhcPs] ->
  -- | Class name
  RdrName ->
  -- | Type variables
  [LHsTyVarBndr GhcPs] ->
  -- | Method signatures
  [LSig GhcPs] ->
  LHsDecl GhcPs
classD_simple ctx clsName clsVars sigs = noLoc (TyClD noExtField cls)
  where
    cls =
      ClassDecl
        { tcdCExt = noExtField,
          tcdCtxt = noLoc ctx,
          tcdLName = noLoc clsName,
          tcdTyVars = mkHsQTvs clsVars,
          tcdFixity = Prefix,
          tcdFDs = [],
          tcdSigs = sigs,
          tcdMeths = emptyBag,
          tcdATs = [],
          tcdATDefs = [],
          tcdDocs = []
        }

-- | A helper for constructing @import qualified@ AST.
qimportD :: ModuleName -> LImportDecl GhcPs
qimportD moduleName = noLoc do
  ImportDecl
    { ideclExt = noExtField,
      ideclSourceSrc = NoSourceText,
      ideclName = noLoc moduleName,
      ideclPkgQual = Nothing,
      ideclSource = False,
      ideclSafe = False,
#if __GLASGOW_HASKELL__ < 810
      ideclQualified = True,
#else
      ideclQualified = QualifiedPre,
#endif
      ideclImplicit = False,
      ideclAs = Nothing,
      ideclHiding = Nothing
    }
