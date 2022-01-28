{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Anonymous.Plugin.Constraints.AllFields (
    CAllFields(..)
  , parseAllFields
  , solveAllFields
  ) where

import Data.Foldable (toList)
import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of @AllFields c r@
data CAllFields = CAllFields {
      -- | Fields of the record (parsed form of @r@)
      allFieldsParsedFields :: Fields

      -- | Raw arguments to @AllFields@ (for evidence construction)
    , allFieldsTypeRaw :: [Type]

      -- | Type of the fields (@r@)
    , allFieldsTypeFields :: Type

      -- | Constraint required for each field (@c@)
    , allFieldsTypeConstraint :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CAllFields where
  ppr (CAllFields parsedFields typeRaw typeConstraint typeFields) = parens $
      text "CAllFields" <+> braces (vcat [
          text "allFieldsParsedFields"   <+> text "=" <+> ppr parsedFields
        , text "allFieldsTypeRaw"        <+> text "=" <+> ppr typeRaw
        , text "allFieldsTypeFields    " <+> text "=" <+> ppr typeFields
        , text "allFieldsTypeConstraint" <+> text "=" <+> ppr typeConstraint
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseAllFields ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CAllFields)
parseAllFields tcs rn@ResolvedNames{..} =
    parseConstraint' clsAllFields $ \case
      args@[r, c] -> do
        fields <- parseFields tcs rn r
        return CAllFields {
            allFieldsParsedFields   = fields
          , allFieldsTypeRaw        = args
          , allFieldsTypeFields     = r
          , allFieldsTypeConstraint = c
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

-- | Construct evidence
--
-- For each field we need an evidence variable corresponding to the evidence
-- that that fields satisfies the constraint.
evidenceAllFields ::
     ResolvedNames
  -> CAllFields
  -> KnownRecord EvVar
  -> TcPluginM 'Solve EvTerm
evidenceAllFields ResolvedNames{..} CAllFields{..} fields = do
    fields' <- mapM dictForField (knownRecordFields fields)
    return $
      evDataConApp
        (classDataCon clsAllFields)
        allFieldsTypeRaw
        [ mkCoreApps (Var idEvidenceAllFields) [
              Type allFieldsTypeConstraint
            , Type allFieldsTypeFields
            , mkListExpr dictType fields'
            ]
        ]
  where
    dictType :: Type
    dictType = mkTyConApp tyConDict [
          liftedTypeKind
        , allFieldsTypeConstraint
        , anyType
        ]

    dictForField :: KnownField EvVar -> TcPluginM 'Solve EvExpr
    dictForField KnownField{ knownFieldType = fieldType
                           , knownFieldInfo = dict
                           } = do
        return $ mkCoreConApps dataConDict [
            Type liftedTypeKind
          , Type allFieldsTypeConstraint
          , Type anyType
          , mkCoreApps (Var idUnsafeCoerce) [
                Type $ mkAppTy allFieldsTypeConstraint fieldType
              , Type $ mkAppTy allFieldsTypeConstraint anyType
              , Var dict
              ]
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveAllFields ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CAllFields
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveAllFields rn orig (L loc cr@CAllFields{..}) = do
    case checkAllFieldsKnown allFieldsParsedFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' :: KnownRecord CtEvidence
           <- knownRecordTraverse fields $ \fld ->
                newWanted loc $
                  mkAppTy allFieldsTypeConstraint (knownFieldType fld)
        ev <- evidenceAllFields rn cr $getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map mkNonCanonical (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"


