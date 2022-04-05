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

import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields, FieldLabel(..))
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.TyConSubst

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
  ppr (CRowHasField label record typeKind typeLabel typeRow typeField) = parens $
      text "CRowHasField" <+> braces (vcat [
          text "hasFieldLabel"     <+> text "=" <+> ppr label
        , text "hasFieldRecord"    <+> text "=" <+> ppr record
        , text "hasFieldTypeKind"  <+> text "=" <+> ppr typeKind
        , text "hasFieldTypeLabel" <+> text "=" <+> ppr typeLabel
        , text "hasFieldTypeRow"   <+> text "=" <+> ppr typeRow
        , text "hasFieldTypeField" <+> text "=" <+> ppr typeField
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
    parseConstraint' clsRowHasField $ \[k, n, r, a] -> do
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

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceHasField ::
     ResolvedNames
  -> CRowHasField
  -> Int        -- ^ Field index
  -> TcPluginM 'Solve EvTerm
evidenceHasField ResolvedNames{..} CRowHasField{..} i = do
    return $
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
    case ParsedRow.lookup name hasFieldRecord of
      Nothing ->
        -- TODO: If the record is fully known, we should issue a custom type
        -- error here rather than leaving the constraint unsolved
        return (Nothing, [])
      Just (i, typ) -> do
        eq <- newWanted loc $
                mkPrimEqPredRole Nominal
                  hasFieldTypeField
                  typ
        ev <- evidenceHasField rn hf i
        return (Just (ev, orig), [mkNonCanonical eq])
