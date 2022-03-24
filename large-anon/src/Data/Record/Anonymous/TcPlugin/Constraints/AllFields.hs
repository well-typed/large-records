{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Record.Anonymous.TcPlugin.Constraints.AllFields (
    CAllFields(..)
  , parseAllFields
  , solveAllFields
  ) where

import Data.Bifunctor
import Data.Foldable (toList)
import Data.Void

import Data.Record.Anonymous.Internal.Row.KnownField (KnownField(..))
import Data.Record.Anonymous.Internal.Row.KnownRow (KnownRow)
import Data.Record.Anonymous.Internal.Row.ParsedRow (Fields)
import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI
import Data.Record.Anonymous.TcPlugin.NameResolution
import Data.Record.Anonymous.TcPlugin.Parsing
import Data.Record.Anonymous.TcPlugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.Row.KnownRow  as KnownRow
import qualified Data.Record.Anonymous.Internal.Row.ParsedRow as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of @AllFields c r@
data CAllFields = CAllFields {
      -- | Fields of the record (parsed form of @r@)
      allFieldsParsedFields :: Fields

      -- | Type of the fields (@r@)
    , allFieldsTypeFields :: Type

      -- | Constraint required for each field (@c@)
    , allFieldsTypeConstraint :: Type

      -- | Constraint argument kind (the @k@ in @c :: k -> Constraint@)
    , allFieldsTypeKind :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CAllFields where
  ppr (CAllFields parsedFields typeConstraint typeKind typeFields) = parens $
      text "CAllFields" <+> braces (vcat [
          text "allFieldsParsedFields"   <+> text "=" <+> ppr parsedFields
        , text "allFieldsTypeFields    " <+> text "=" <+> ppr typeFields
        , text "allFieldsTypeConstraint" <+> text "=" <+> ppr typeConstraint
        , text "allFieldsTypeKind"       <+> text "=" <+> ppr typeKind
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
      [k, r, c] -> do
        fields <- ParsedRow.parseFields tcs rn r
        return CAllFields {
            allFieldsParsedFields   = fields
          , allFieldsTypeFields     = r
          , allFieldsTypeConstraint = c
          , allFieldsTypeKind       = k
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
  -> KnownRow (Type, EvVar)
  -> TcPluginM 'Solve EvTerm
evidenceAllFields ResolvedNames{..} CAllFields{..} fields = do
    fields' <- mapM dictForField (KnownRow.toList fields)
    return $
      evDataConApp
        (classDataCon clsAllFields)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceAllFields) $ concat [
              map Type typeArgsEvidence
            , [mkListExpr (mkTyConApp tyConDict typeArgsDict) fields']
            ]
        ]
  where
    -- Type arguments to @Dict@ and to @AllFields@
    typeArgsDict, typeArgsEvidence :: [Type]
    typeArgsDict = [
          allFieldsTypeKind
        , allFieldsTypeConstraint
        , anyAtKind
        ]
    typeArgsEvidence = [
          allFieldsTypeKind
        , allFieldsTypeFields
        , allFieldsTypeConstraint
        ]

    dictForField :: KnownField (Type, EvVar) -> TcPluginM 'Solve EvExpr
    dictForField KnownField{ knownFieldInfo = (fieldType, dict) } = do
        return $ mkCoreConApps dataConDict $ concat [
            map Type typeArgsDict
          , [ mkCoreApps (Var idUnsafeCoerce) [
                Type $ mkAppTy allFieldsTypeConstraint fieldType
              , Type $ mkAppTy allFieldsTypeConstraint anyAtKind
              , Var dict
              ]
            ]
          ]

    -- Any at kind @k@
    anyAtKind :: Type
    anyAtKind = mkTyConApp anyTyCon [allFieldsTypeKind]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveAllFields ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CAllFields
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveAllFields rn orig (L loc cr@CAllFields{..}) = do
    case ParsedRow.allKnown allFieldsParsedFields of
      Nothing ->
        return (Nothing, [])
      Just fields -> do
        fields' :: KnownRow (Type, CtEvidence)
           <- KnownRow.traverse fields $ \_nm typ -> fmap (typ,) $
                newWanted loc $
                  mkAppTy allFieldsTypeConstraint typ
        ev <- evidenceAllFields rn cr $ second getEvVar <$> fields'
        return (
            Just (ev, orig)
          , map (mkNonCanonical . snd) (toList fields')
          )
  where
    getEvVar :: CtEvidence -> EvVar
    getEvVar ct = case ctev_dest ct of
      EvVarDest var -> var
      HoleDest  _   -> error "impossible (we don't ask for primitive equality)"


