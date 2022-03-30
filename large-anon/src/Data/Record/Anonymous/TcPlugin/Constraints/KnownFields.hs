{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.TcPlugin.Constraints.KnownFields (
    CKnownFields(..)
  , parseKnownFields
  , solveKnownFields
  ) where

import Data.Void

import Data.Record.Anonymous.Internal.Row.KnownRow (KnownRow)
import Data.Record.Anonymous.Internal.Row.ParsedRow (Fields)
import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI
import Data.Record.Anonymous.TcPlugin.NameResolution
import Data.Record.Anonymous.TcPlugin.Parsing
import Data.Record.Anonymous.TcPlugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.Row.KnownField as KnownField
import qualified Data.Record.Anonymous.Internal.Row.KnownRow   as KnownRow
import qualified Data.Record.Anonymous.Internal.Row.ParsedRow  as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @KnownFields (r :: [(Symbol, Kind)]) @ constraint
data CKnownFields = CKnownFields {
      -- | Fields of the record
      knownFieldsParsedFields :: Fields

      -- | Type of the record fields (@r@)
    , knownFieldsTypeRecord :: Type

      -- | Kind of the type information (@k@)
    , knownFieldsTypeKind :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CKnownFields where
  ppr (CKnownFields parsedFields typeRecord typeKind) = parens $
      text "CKnownFields" <+> braces (vcat [
          text "knownFieldsParsedFields" <+> text "=" <+> ppr parsedFields
        , text "knownFieldsTypeRecord"   <+> text "=" <+> ppr typeRecord
        , text "knownFieldsTypeKind"     <+> text "=" <+> ppr typeKind
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
      [k, r] -> do
        fields <- ParsedRow.parseFields tcs rn r
        return CKnownFields {
            knownFieldsParsedFields = fields
          , knownFieldsTypeRecord   = r
          , knownFieldsTypeKind     = k
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
  -> KnownRow a
  -> TcPluginM 'Solve EvTerm
evidenceKnownFields ResolvedNames{..} CKnownFields{..} r = do
    fields <- mapM KnownField.toExpr (KnownRow.toList r)
    return $
      evDataConApp
        (classDataCon clsKnownFields)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceKnownFields) $ concat [
              map Type typeArgsEvidence
            , [ mkListExpr stringTy fields ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          knownFieldsTypeKind
        , knownFieldsTypeRecord
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveKnownFields ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CKnownFields
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveKnownFields rn orig (L _ cm@CKnownFields{..}) = do
    -- See 'solveRecordConstraints' for a discussion of 'allFieldsKnown'
    case ParsedRow.allKnown knownFieldsParsedFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        ev <- evidenceKnownFields rn cm fields
        return (Just (ev, orig), [])
