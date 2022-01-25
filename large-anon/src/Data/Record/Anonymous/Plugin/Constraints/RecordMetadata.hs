{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordMetadata (
    CRecordMetadata(..)
  , parseRecordMetadata
  , solveRecordMetadata
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

-- | Parsed form of a @RecordMetadata r@ constraint
data CRecordMetadata = CRecordMetadata {
      -- | Fields of the record
      recordMetadataFields :: Fields

      -- | Type of the record (@r@)
      --
      -- This is the only type argument to @RecordMetadata@, so we don't
      -- separately record the "raw arguments|/
    , recordMetadataTypeRecord :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CRecordMetadata where
  ppr (CRecordMetadata fields typeRecord) = parens $
          text "CRecordMetadata"
      <+> ppr fields
      <+> ppr typeRecord

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRecordMetadata ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRecordMetadata)
parseRecordMetadata tcs rn@ResolvedNames{..} =
    parseConstraint' clsRecordMetadata $ \case
      [r] -> do
        fields <- parseFields tcs rn r
        return CRecordMetadata {
            recordMetadataFields     = fields
          , recordMetadataTypeRecord = r
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
evidenceRecordMetadata ::
     ResolvedNames
  -> CRecordMetadata
  -> KnownRecord EvVar
  -> TcPluginM 'Solve EvTerm
evidenceRecordMetadata ResolvedNames{..}
                       CRecordMetadata{..}
                       r
                     = do
    nameRecord <- mkStringExpr "Record"
    nameConstr <- mkStringExpr "Record"
    return $
      evDataConApp
        (classDataCon clsRecordMetadata)
        [recordMetadataTypeRecord]
        [ mkCoreConApps dataConMetadata [
              Type $ mkTyConApp tyConRecord [recordMetadataTypeRecord]
            , nameRecord
            , nameConstr
            , mkUncheckedIntExpr (fromIntegral (length (knownRecordFields r)))
            , mkCoreApps (Var idUnsafeFieldMetadata) [
                  Type recordMetadataTypeRecord
                , mkListExpr fieldMetadataType $
                    map mkFieldInfoAny (knownRecordFields r)
                ]
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
            -- TODO: Think about strict/lazy fields
          , mkCoreConApps dataConFieldLazy []
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRecordMetadata ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRecordMetadata
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRecordMetadata rn@ResolvedNames{..}
                       orig
                       (L loc cm@CRecordMetadata{..})
                     = do
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case checkAllFieldsKnown recordMetadataFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' <- knownRecordTraverse fields $ \fld -> do
                     newWanted loc $
                       mkClassPred clsKnownSymbol [
                           mkStrLitTy (knownFieldName fld)
                         ]
        ev <- evidenceRecordMetadata rn cm $ getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map mkNonCanonical (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"
