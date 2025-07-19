{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.RowHasField (
    CRowHasField(..)
  , parseRowHasField
  , solveRowHasField
  ) where

import Data.Void
import GHC.Stack

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (KnownRowField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields, FieldLabel(..))

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow  as KnownRow
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of a @RowHasField n r@ with @r :: Row k@ constraint
data CRowHasField = CRowHasField {
      -- | Label we're looking for (@n@)
      hasFieldLabel :: FieldLabel

      -- | Fields of the record (parsed form of @r@)
      --
      -- These may be fully or partially known, or completely unknown.
    , hasFieldRecord :: Fields

      -- | Row kind (@k@)
    , hasFieldTypeKind :: Type

      -- | Record field (@n@)
    , hasFieldTypeLabel :: Type

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

instance Outputable CRowHasField where
  ppr CRowHasField{..} = parens $
      text "CRowHasField" <+> braces (vcat [
          text "hasFieldLabel"     <+> text "=" <+> ppr hasFieldLabel
        , text "hasFieldRecord"    <+> text "=" <+> ppr hasFieldRecord
        , text "hasFieldTypeKind"  <+> text "=" <+> ppr hasFieldTypeKind
        , text "hasFieldTypeLabel" <+> text "=" <+> ppr hasFieldTypeLabel
        , text "hasFieldTypeRow"   <+> text "=" <+> ppr hasFieldTypeRow
        , text "hasFieldTypeField" <+> text "=" <+> ppr hasFieldTypeField
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseRowHasField ::
     HasCallStack
  => TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CRowHasField)
parseRowHasField tcs rn@ResolvedNames{..} =
    parseConstraint' clsRowHasField $ \ args ->
      case args of
        [k, n, r, a] -> do
          label  <- ParsedRow.parseFieldLabel n
          fields <- ParsedRow.parseFields tcs rn r
          return $ CRowHasField {
              hasFieldLabel     = label
            , hasFieldRecord    = fields
            , hasFieldTypeKind  = k
            , hasFieldTypeLabel = n
            , hasFieldTypeRow   = r
            , hasFieldTypeField = a
            }
        _ -> pprPanic "parseRowHasField: expected 4 arguments" $
               ( text "args:" <+> ppr args )

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CRowHasField
  -> Int        -- ^ Field index
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CRowHasField{..} i = do
    return $ EvExpr $
      evDataConApp
        (classDataCon clsRowHasField)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceRowHasField) $ concat [
              map Type typeArgsEvidence
            , [mkUncheckedIntExpr $ fromIntegral i]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          hasFieldTypeKind
        , hasFieldTypeLabel
        , hasFieldTypeRow
        , hasFieldTypeField
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveRowHasField ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CRowHasField
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveRowHasField _ _ (L _ CRowHasField{hasFieldLabel = FieldVar _}) =
    return (Nothing, [])
solveRowHasField rn orig (L loc hf@CRowHasField{hasFieldLabel = FieldKnown name, ..}) =
    case ParsedRow.allKnown hasFieldRecord of
      Nothing ->
        -- Not all fields are known; leave the constraint unsolved
        return (Nothing, [])
      Just allKnown ->
        case KnownRow.lookup name allKnown of
          Nothing ->
            -- TODO: We should issue an error here rather than leaving the
            -- constraint unsolved: we /know/ the field does not exist
            return (Nothing, [])
          Just info -> do
            eq <- newWanted loc $
                    mkEqPredRole Nominal
                      hasFieldTypeField
                      (knownRowFieldInfo info)
            ev <- evidenceHasField rn hf (knownRowFieldIndex info)
            return (Just (ev, orig), [mkNonCanonical eq])
