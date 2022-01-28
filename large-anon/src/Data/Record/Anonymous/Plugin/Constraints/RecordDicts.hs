{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordDicts (
    CRecordDicts(..)
  , parseRecordDicts
  , solveRecordDicts
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

-- | Parsed form of @RecordDicts c r@
data CRecordDicts = CRecordDicts {
      -- | Fields of the record (parsed form of @r@)
      recordDictsFields :: Fields

      -- | Raw arguments to @RecordDicts@ (for evidence construction)
    , recordDictsTypeRaw :: [Type]

      -- | Type of the fields (@r@)
    , recordDictsTypeFields :: Type

      -- | Constraint required for each field (@c@)
    , recordDictsTypeConstraint :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CRecordDicts where
  ppr (CRecordDicts fields typeRaw typeConstraint typeFields) = parens $
      text "CRecordDicts" <+> braces (vcat [
          text "recordDictsFields"         <+> text "=" <+> ppr fields
        , text "recordDictsTypeRaw"        <+> text "=" <+> ppr typeRaw
        , text "recordDictsTypeFields    " <+> text "=" <+> ppr typeFields
        , text "recordDictsTypeConstraint" <+> text "=" <+> ppr typeConstraint
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRecordDicts ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRecordDicts)
parseRecordDicts tcs rn@ResolvedNames{..} =
    parseConstraint' clsRecordDicts $ \case
      args@[r, c] -> do
        fields <- parseFields tcs rn r
        return CRecordDicts {
            recordDictsFields         = fields
          , recordDictsTypeRaw        = args
          , recordDictsTypeFields     = r
          , recordDictsTypeConstraint = c
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
evidenceRecordDicts ::
     ResolvedNames
  -> CRecordDicts
  -> KnownRecord EvVar
  -> TcPluginM 'Solve EvTerm
evidenceRecordDicts ResolvedNames{..} CRecordDicts{..} fields = do
    fields' <- mapM dictForField (knownRecordFields fields)
    return $
      evDataConApp
        (classDataCon clsRecordDicts)
        recordDictsTypeRaw
        [ mkCoreApps (Var idUnsafeRecordDicts) [
              Type recordDictsTypeConstraint
            , Type recordDictsTypeFields
            , mkListExpr (mkTupleTy Boxed [stringTy, dictType]) fields'
            ]
        ]
  where
    dictType :: Type
    dictType = mkTyConApp tyConDict [
          liftedTypeKind
        , recordDictsTypeConstraint
        , anyType
        ]

    dictForField :: KnownField EvVar -> TcPluginM 'Solve EvExpr
    dictForField KnownField{ knownFieldName = name
                           , knownFieldType = fieldType
                           , knownFieldInfo = dict
                           } = do
        str <- mkStringExprFS name
        return $ mkCoreTup [
            str
          , mkCoreConApps dataConDict [
                Type liftedTypeKind
              , Type recordDictsTypeConstraint
              , Type anyType
              , mkCoreApps (Var idUnsafeCoerce) [
                    Type $ mkAppTy recordDictsTypeConstraint fieldType
                  , Type $ mkAppTy recordDictsTypeConstraint anyType
                  , Var dict
                  ]
              ]
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRecordDicts ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRecordDicts
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRecordDicts rn orig (L loc cr@CRecordDicts{..}) = do
    case checkAllFieldsKnown recordDictsFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' :: KnownRecord CtEvidence
           <- knownRecordTraverse fields $ \fld ->
                newWanted loc $
                  mkAppTy recordDictsTypeConstraint (knownFieldType fld)
        ev <- evidenceRecordDicts rn cr $getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map mkNonCanonical (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"


