{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRow (
    CSubRow(..)
  , parseSubRow
  , solveSubRow
  ) where

import Control.Monad (forM)
import Data.Void

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (Source(..), Target (..), KnownRowField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)

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
  ppr CSubRow{..} = parens $
      text "CSubRow" <+> braces (vcat [
          text "subrowParsedLHS"   <+> text "=" <+> ppr subrowParsedLHS
        , text "subrowParsedRHS"   <+> text "=" <+> ppr subrowParsedRHS
        , text "subrowTypeLHS"     <+> text "=" <+> ppr subrowTypeLHS
        , text "subrowTypeRHS"     <+> text "=" <+> ppr subrowTypeRHS
        , text "subrowTypeKind"    <+> text "=" <+> ppr subrowTypeKind
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
    parseConstraint' clsSubRow $ \ args ->
      case args of
        [subrowTypeKind, subrowTypeLHS, subrowTypeRHS] -> do
          subrowParsedLHS <- ParsedRow.parseFields tcs rn subrowTypeLHS
          subrowParsedRHS <- ParsedRow.parseFields tcs rn subrowTypeRHS
          return $ CSubRow {..}
        _ -> pprPanic "parseSubRow: expected 3 arguments" $
               text "args" <+> ppr args

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceSubRow ::
     ResolvedNames
  -> CSubRow
  -> [(Target (KnownField Type), Source (KnownRowField Type))]
  -> TcPluginM 'Solve EvTerm
evidenceSubRow ResolvedNames{..} CSubRow{..} fields = do
    return $ EvExpr $
      evDataConApp
        (classDataCon clsSubRow)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceSubRow) $ concat [
              map Type typeArgsEvidence
            , [ mkListExpr intTy $
                  map (mkUncheckedIntExpr . fromIntegral) indices ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          subrowTypeKind
        , subrowTypeLHS
        , subrowTypeRHS
        ]

    -- Indices into the source array, in the order of the target array
    indices :: [Int]
    indices = map (knownRowFieldIndex . getSource . snd) fields

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
        case rhs `KnownRow.isSubRowOf` lhs of
          Right inBoth -> do
            eqs <- forM inBoth $ \(Target r, Source l) -> newWanted loc $
                     mkEqPredRole
                       Nominal
                       (knownRowFieldInfo l)
                       (knownFieldInfo r)
            ev  <- evidenceSubRow rn proj inBoth
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _err ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])
