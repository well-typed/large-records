{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.AllFields (
    CAllFields(..)
  , parseAllFields
  , solveAllFields
  ) where

import Data.Bifunctor
import Data.Void

import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (KnownRow)
import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow  as KnownRow
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow as ParsedRow

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
  ppr CAllFields{..} = parens $
      text "CAllFields" <+> braces (vcat [
          text "allFieldsParsedFields"   <+> text "=" <+> ppr allFieldsParsedFields
        , text "allFieldsTypeFields    " <+> text "=" <+> ppr allFieldsTypeFields
        , text "allFieldsTypeConstraint" <+> text "=" <+> ppr allFieldsTypeConstraint
        , text "allFieldsTypeKind"       <+> text "=" <+> ppr allFieldsTypeKind
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
  -> KnownRow (Type, EvExpr)
  -> TcPluginM 'Solve EvTerm
evidenceAllFields ResolvedNames{..} CAllFields{..} fields = do
    fields' <- mapM dictForField (KnownRow.inRowOrder fields)
    return $ EvExpr $
      evDataConApp
        (classDataCon clsAllFields)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceAllFields) $ concat [
              map Type typeArgsEvidence
            , [mkListExpr (mkTyConApp tyConDictAny typeArgsDict) fields']
            ]
        ]
  where
    -- Type arguments to @Dict@ and to @AllFields@
    typeArgsDict, typeArgsEvidence :: [Type]
    typeArgsDict = [
          allFieldsTypeKind
        , allFieldsTypeConstraint
        ]
    typeArgsEvidence = [
          allFieldsTypeKind
        , allFieldsTypeFields
        , allFieldsTypeConstraint
        ]

    dictForField :: KnownField (Type, EvExpr) -> TcPluginM 'Solve EvExpr
    dictForField KnownField{ knownFieldInfo = (fieldType, dict) } = do
        return $ mkCoreApps (Var idMkDictAny) $ concat [
            map Type (typeArgsDict ++ [fieldType])
          , [dict]
          ]

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
           <- KnownRow.traverse fields $ \_nm _ix typ -> fmap (typ,) $
                newWanted loc $
                  mkAppTy allFieldsTypeConstraint typ
        ev <- evidenceAllFields rn cr $ second ctEvExpr <$> fields'
        return (
            Just (ev, orig)
          , map (mkNonCanonical . snd . knownFieldInfo) $
              KnownRow.inRowOrder fields'
          )
