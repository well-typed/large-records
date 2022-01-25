{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.RecordConstraints (
    CRecordConstraints(..)
  , parseRecordConstraints
  , solveRecordConstraints
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

-- | Parsed form of @RecordConstraints f r c@
data CRecordConstraints = CRecordConstraints {
      -- | Fields of the record (parsed form of @r@)
      recordConstraintsFields :: Fields

      -- | Raw arguments to @RecordConstraints@ (for evidence construction)
    , recordConstraintsTypeRaw :: [Type]

      -- | Type of the functor (@f@)
    , recordConstraintsTypeFunctor :: Type

      -- | Type of the record fields (@r@)
    , recordConstraintsTypeRecord :: Type

      -- | Constraint that we need for every field (@c@)
    , recordConstraintsTypeConstraint :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CRecordConstraints where
  ppr (CRecordConstraints fields typeRaw typeRecord typeFunctor typeConstraint) = parens $
      text "CRecordConstraints" <+> braces (vcat [
          text "recordConstraintsFields"         <+> text "=" <+> ppr fields
        , text "recordConstraintsTypeRaw"        <+> text "=" <+> ppr typeRaw
        , text "recordConstraintsTypeFunctor"    <+> text "=" <+> ppr typeFunctor
        , text "recordConstraintsTypeRecord"     <+> text "=" <+> ppr typeRecord
        , text "recordConstraintsTypeConstraint" <+> text "=" <+> ppr typeConstraint
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRecordConstraints ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRecordConstraints)
parseRecordConstraints tcs rn@ResolvedNames{..} =
    parseConstraint' clsRecordConstraints $ \case
      args@[f, r, c] -> do
        fields <- parseFields tcs rn r
        return CRecordConstraints {
            recordConstraintsFields         = fields
          , recordConstraintsTypeRaw        = args
          , recordConstraintsTypeFunctor    = f
          , recordConstraintsTypeRecord     = r
          , recordConstraintsTypeConstraint = c
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
evidenceRecordConstraints ::
     ResolvedNames
  -> CRecordConstraints
  -> EvVar -- Evidence of RecordMetadata
  -> KnownRecord EvVar
  -> TcPluginM 'Solve EvTerm
evidenceRecordConstraints ResolvedNames{..}
                          CRecordConstraints{..}
                          evMeta
                          fields
                        = do
    return $
      evDataConApp
        (classDataCon clsRecordConstraints)
        recordConstraintsTypeRaw
        [ Var evMeta
        , mkCoreApps (Var idUnsafeDictRecord) [
              Type recordConstraintsTypeFunctor
            , Type recordConstraintsTypeRecord
            , Type recordConstraintsTypeConstraint
            , mkListExpr dictType $ map mkDictAny (knownRecordFields fields)
            ]
        ]
  where
    dictType :: Type
    dictType = mkTyConApp tyConDict [
          liftedTypeKind
        , recordConstraintsTypeConstraint
        , recordConstraintsTypeFunctor `mkAppTy` anyType
        ]

    mkDictAny :: KnownField EvVar -> EvExpr
    mkDictAny KnownField{ knownFieldType = fieldType
                        , knownFieldInfo = dict
                        } =
        mkCoreConApps dataConDict [
            Type liftedTypeKind
          , Type recordConstraintsTypeConstraint
          , Type (recordConstraintsTypeFunctor `mkAppTy` anyType)
          , mkCoreApps (Var idUnsafeCoerce) [
                Type $ mkAppTy recordConstraintsTypeConstraint (recordConstraintsTypeFunctor `mkAppTy` fieldType)
              , Type $ mkAppTy recordConstraintsTypeConstraint (recordConstraintsTypeFunctor `mkAppTy` anyType)
              , Var dict
              ]
          ]

    -- Any at kind Type
    anyType :: Type
    anyType = mkTyConApp anyTyCon [liftedTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRecordConstraints ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRecordConstraints
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRecordConstraints rn@ResolvedNames{clsRecordMetadata}
                       orig
                       (L loc cr@CRecordConstraints{..})
                     = do
    -- The call to 'allFieldsKnown' establishes two things:
    --
    -- o Unless all fields of the record are known, we cannot construct the
    --   appropriate dictionary.
    -- o Moreover, that dictionary is a vector of dictionaries, one per field,
    --   and the order is determined by the field names (see also the 'Generic'
    --   instance for 'Record'). The 'Map' constructed by 'allFieldsKnown'
    --   gives us this ordering.
    case checkAllFieldsKnown recordConstraintsFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        -- RecordConstraints has a superclass constraint on RecordMetadata
        evMeta  <- newWanted loc $
                     mkClassPred
                       clsRecordMetadata
                       [ recordConstraintsTypeFunctor
                       , recordConstraintsTypeRecord
                       ]
        fields' <- knownRecordTraverse fields $ \fld ->
                     newWanted loc $
                       mkAppTy
                         recordConstraintsTypeConstraint
                         (recordConstraintsTypeFunctor `mkAppTy` knownFieldType fld)
        ev      <- evidenceRecordConstraints rn cr (getEvVar evMeta) $
                     getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map mkNonCanonical (evMeta : toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"

