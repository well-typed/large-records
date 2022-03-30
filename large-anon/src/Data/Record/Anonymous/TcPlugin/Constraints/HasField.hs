{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.TcPlugin.Constraints.HasField (
    CHasField(..)
  , parseHasField
  , solveHasField
  ) where

import Data.Void
import GHC.Stack

import Data.Record.Anon.Core.FieldName

import Data.Record.Anonymous.Internal.Row.ParsedRow (Fields, FieldLabel(..))
import Data.Record.Anonymous.TcPlugin.GhcTcPluginAPI
import Data.Record.Anonymous.TcPlugin.NameResolution
import Data.Record.Anonymous.TcPlugin.Parsing
import Data.Record.Anonymous.TcPlugin.TyConSubst

import qualified Data.Record.Anonymous.Internal.Row.ParsedRow as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @HasField n f r a@ constraint
data CHasField = CHasField {
      -- | Label we're looking for (@x@)
      hasFieldLabel :: FieldLabel

      -- | Fields of the record (parsed form of @r'@)
      --
      -- These may be fully or partially known, or completely unknown.
    , hasFieldRecord :: Fields

      -- | Functor argument kind (the @k@ in @f :: k -> Type@)
    , hasFieldTypeKind :: Type

      -- | Record field (@n@)
    , hasFieldTypeLabel :: Type

      -- | Record functor argument (@f@)
    , hasFieldTypeFunctor :: Type

      -- | Row (@r@)
    , hasFieldTypeRow :: Type

      -- | Type of the record field we're looking for (@a@)
      --
      -- Although @a@ will be of the form @f a'@ for some @a'@, we do not
      -- enforce that here (but instead generate a new wanted equality
      -- constraint to enforce this).
    , hasFieldTypeField :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CHasField where
  ppr (CHasField label record typeKind typeLabel typeFunctor typeRow typeField) = parens $
      text "CHasField" <+> braces (vcat [
          text "hasFieldLabel"       <+> text "=" <+> ppr label
        , text "hasFieldRecord"      <+> text "=" <+> ppr record
        , text "hasFieldTypeKind"    <+> text "=" <+> ppr typeKind
        , text "hasFieldTypeLabel"   <+> text "=" <+> ppr typeLabel
        , text "hasFieldTypeFunctor" <+> text "=" <+> ppr typeFunctor
        , text "hasFieldTypeRow"     <+> text "=" <+> ppr typeRow
        , text "hasFieldTypeField"   <+> text "=" <+> ppr typeField
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseHasField ::
     HasCallStack
  => TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CHasField)
parseHasField tcs rn@ResolvedNames{..} =
    parseConstraint' clsRecordHasField $ \[k, n, f, r, a] -> do
      label  <- ParsedRow.parseFieldLabel n
      fields <- ParsedRow.parseFields tcs rn r

      return $ CHasField {
          hasFieldLabel       = label
        , hasFieldRecord      = fields
        , hasFieldTypeKind    = k
        , hasFieldTypeLabel   = n
        , hasFieldTypeFunctor = f
        , hasFieldTypeRow     = r
        , hasFieldTypeField   = a
        }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CHasField
  -> Int        -- ^ Field index
  -> FieldName  -- ^ Field name
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CHasField{..} i FieldName{..} = do
    name' <- mkStringExpr fieldNameLabel
    return $
      evDataConApp
        (classDataCon clsRecordHasField)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceRecordHasField) $ concat [
              map Type typeArgsEvidence
            , [ mkUncheckedIntExpr (fromIntegral i)
              , mkCoreTup [
                   mkUncheckedIntExpr (fromIntegral fieldNameHash)
                 , name'
                 ]
              ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          hasFieldTypeKind
        , hasFieldTypeLabel
        , hasFieldTypeFunctor
        , hasFieldTypeRow
        , hasFieldTypeField
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveHasField ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CHasField
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveHasField _ _ (L _ CHasField{hasFieldLabel = FieldVar _}) =
    return (Nothing, [])
solveHasField rn orig (L loc hf@CHasField{hasFieldLabel = FieldKnown name, ..}) =
    case ParsedRow.lookup name hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return (Nothing, [])
      Just (i, typ) -> do
        eq <- newWanted loc $
                mkPrimEqPredRole Nominal
                  hasFieldTypeField
                  (hasFieldTypeFunctor `mkAppTy` typ)
        ev <- evidenceHasField rn hf i name
        return (Just (ev, orig), [mkNonCanonical eq])
