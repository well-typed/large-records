{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRow (
    CSubRow(..)
  , parseSubRow
  , solveSubRow
  ) where

import Control.Monad (forM)
import Data.Void

import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.TyConSubst

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow  as KnownRow
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of @SubRow@
--
-- > SubRow (r :: [(Symbol, k)]) (r' :: [(Symbol, k)])
data CSubRow = CSubRow {
      -- | Fields on the LHS
      subrowParsedLHS :: Fields

      -- | Fields on the RHS
    , subrowParsedRHS :: Fields

      -- | Left-hand side (@r@)
    , subrowTypeLHS :: Type

      -- | Right-hand side (@r'@)
    , subrowTypeRHS :: Type

      -- | Functor argument kind (@k@)
    , subrowTypeKind :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CSubRow where
  ppr (CSubRow parsedLHS parsedRHS typeLHS typeRHS typeKind) = parens $
      text "CSubRow" <+> braces (vcat [
          text "subrowParsedLHS"   <+> text "=" <+> ppr parsedLHS
        , text "subrowParsedRHS"   <+> text "=" <+> ppr parsedRHS
        , text "subrowTypeLHS"     <+> text "=" <+> ppr typeLHS
        , text "subrowTypeRHS"     <+> text "=" <+> ppr typeRHS
        , text "subrowTypeKind"    <+> text "=" <+> ppr typeKind
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseSubRow ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CSubRow)
parseSubRow tcs rn@ResolvedNames{..} =
    parseConstraint' clsSubRow $ \[typeKind, typeLHS, typeRHS] -> do
      fieldsLHS <- ParsedRow.parseFields tcs rn typeLHS
      fieldsRHS <- ParsedRow.parseFields tcs rn typeRHS
      return $ CSubRow {
            subrowParsedLHS = fieldsLHS
          , subrowParsedRHS = fieldsRHS
          , subrowTypeLHS   = typeLHS
          , subrowTypeRHS   = typeRHS
          , subrowTypeKind  = typeKind
          }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceSubRow ::
     ResolvedNames
  -> CSubRow
  -> [Int]
  -> TcPluginM 'Solve EvTerm
evidenceSubRow ResolvedNames{..} CSubRow{..} fields = do
    return $
      evDataConApp
        (classDataCon clsSubRow)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceSubRow) $ concat [
              map Type typeArgsEvidence
            , [ mkListExpr intTy $
                  map (mkUncheckedIntExpr . fromIntegral) fields ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          subrowTypeKind
        , subrowTypeLHS
        , subrowTypeRHS
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveSubRow ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CSubRow
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveSubRow rn orig (L loc proj@CSubRow{..}) =
    case ( ParsedRow.allKnown subrowParsedLHS
         , ParsedRow.allKnown subrowParsedRHS
         ) of
      (Just lhs, Just rhs) ->
        case KnownRow.isSubRow lhs rhs of
          Right inBoth -> do
            eqs <- forM inBoth $ \(_i, (l, r)) ->
                     newWanted loc $ mkPrimEqPredRole Nominal l r
            ev  <- evidenceSubRow rn proj (map fst inBoth)
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _err ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])
