{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.KnownFields (
    CKnownFields(..)
  , parseKnownFields
  , solveKnownFields
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

-- | Parsed form of a @KnownFields r@ constraint
data CKnownFields = CKnownFields {
      -- | Fields of the record
      knownFieldsParsedFields :: Fields

      -- | Raw arguments to @KnownFields@ (for evidence construction)
    , knownFieldsTypeRaw :: [Type]

      -- | Type of the record fields (@r@)
    , knownFieldsTypeRecord :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CKnownFields where
  ppr (CKnownFields parsedFields typeRaw typeRecord) = parens $
      text "CKnownFields" <+> braces (vcat [
          text "knownFieldsParsedFields" <+> text "+" <+> ppr parsedFields
        , text "knownFieldsTypeRaw"      <+> text "+" <+> ppr typeRaw
        , text "knownFieldsTypeRecord"   <+> text "+" <+> ppr typeRecord
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseKnownFields ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CKnownFields)
parseKnownFields tcs rn@ResolvedNames{..} =
    parseConstraint' clsKnownFields $ \case
      args@[r] -> do
        fields <- parseFields tcs rn r
        return CKnownFields {
            knownFieldsParsedFields = fields
          , knownFieldsTypeRaw      = args
          , knownFieldsTypeRecord   = r
          }
      _invalidNumArgs ->
        Nothing

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

-- | Construct evidence
--
-- For each field we need an evidence variable corresponding to the evidence
-- that that field name satisfies KnownSymbol.
evidenceKnownFields ::
     ResolvedNames
  -> CKnownFields
  -> KnownRecord EvVar
  -> TcPluginM 'Solve EvTerm
evidenceKnownFields ResolvedNames{..}
                       CKnownFields{..}
                       r
                     = do
    return $
      evDataConApp
        (classDataCon clsKnownFields)
        [knownFieldsTypeRecord]
        [ mkCoreApps (Var idEvidenceKnownFields) [
              Type knownFieldsTypeRecord
            , mkListExpr fieldMetadataType $
                map mkFieldInfoAny (knownRecordFields r)
            ]
        ]
  where
    fieldMetadataType :: Type
    fieldMetadataType = mkTyConApp tyConFieldMetadata [anyType]

    mkFieldInfoAny :: KnownField EvVar -> EvExpr
    mkFieldInfoAny KnownField{knownFieldName = name, knownFieldInfo = dict} =
        mkCoreConApps dataConFieldMetadata [
            Type anyType
          , Type (mkStrLitTy name)
          , Var dict
          , mkCoreConApps dataConProxy [
                Type $ mkTyConTy typeSymbolKindCon
              , Type $ mkStrLitTy name
              ]
            -- @large-anon@ currently only supports records with strict fields
          , mkCoreConApps dataConFieldStrict []
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveKnownFields ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CKnownFields
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveKnownFields rn@ResolvedNames{..}
                       orig
                       (L loc cm@CKnownFields{..})
                     = do
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case checkAllFieldsKnown knownFieldsParsedFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' <- knownRecordTraverse fields $ \fld -> do
                     newWanted loc $
                       mkClassPred clsKnownSymbol [
                           mkStrLitTy (knownFieldName fld)
                         ]
        ev <- evidenceKnownFields rn cm $ getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map mkNonCanonical (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"
