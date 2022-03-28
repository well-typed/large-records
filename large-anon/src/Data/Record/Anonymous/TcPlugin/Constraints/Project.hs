{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.TcPlugin.Constraints.Project (
    CProject(..)
  , parseProject
  , solveProject
  ) where

import Control.Monad (forM)
import Data.Void

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

-- | Parsed form of @Project@
--
-- > Project f (r :: [(Symbol, k)]) (r' :: [(Symbol, k)])
data CProject = CProject {
      -- | Fields on the LHS
      projectParsedLHS :: Fields

      -- | Fields on the RHS
    , projectParsedRHS :: Fields

      -- | Functor argument (@f@)
    , projectTypeFunctor :: Type

      -- | Left-hand side of the projection (@r@)
    , projectTypeLHS :: Type

      -- | Right-hand side of the projection (@r'@)
    , projectTypeRHS :: Type

      -- | Functor argument kind (@k@)
    , projectTypeKind :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CProject where
  ppr (CProject parsedLHS parsedRHS typeFunctor typeLHS typeRHS typeKind) = parens $
      text "CProject" <+> braces (vcat [
          text "projectParsedLHS"   <+> text "=" <+> ppr parsedLHS
        , text "projectParsedRHS"   <+> text "=" <+> ppr parsedRHS
        , text "projectTypeFunctor" <+> text "=" <+> ppr typeFunctor
        , text "projectTypeLHS"     <+> text "=" <+> ppr typeLHS
        , text "projectTypeRHS"     <+> text "=" <+> ppr typeRHS
        , text "projectTypeKind"    <+> text "=" <+> ppr typeKind
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseProject ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CProject)
parseProject tcs rn@ResolvedNames{..} =
    parseConstraint' clsProject $ \[typeKind, typeFunctor, typeLHS, typeRHS] -> do
      fieldsLHS <- ParsedRow.parseFields tcs rn typeLHS
      fieldsRHS <- ParsedRow.parseFields tcs rn typeRHS
      return $ CProject {
            projectParsedLHS   = fieldsLHS
          , projectParsedRHS   = fieldsRHS
          , projectTypeFunctor = typeFunctor
          , projectTypeLHS     = typeLHS
          , projectTypeRHS     = typeRHS
          , projectTypeKind    = typeKind
          }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceProject ::
     ResolvedNames
  -> CProject
  -> [Int]
  -> TcPluginM 'Solve EvTerm
evidenceProject ResolvedNames{..} CProject{..} fields = do
    return $
      evDataConApp
        (classDataCon clsProject)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceProject) $ concat [
              map Type typeArgsEvidence
            , [ mkListExpr intTy $
                  map (mkUncheckedIntExpr . fromIntegral) fields ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          projectTypeKind
        , projectTypeFunctor
        , projectTypeLHS
        , projectTypeRHS
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveProject ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CProject
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveProject rn orig (L loc proj@CProject{..}) =
    case ( ParsedRow.allKnown projectParsedLHS
         , ParsedRow.allKnown projectParsedRHS
         ) of
      (Just lhs, Just rhs) ->
        case KnownRow.canProject lhs rhs of
          Right inBoth -> do
            eqs <- forM inBoth $ \(_i, (l, r)) ->
                     newWanted loc $ mkPrimEqPredRole Nominal l r
            ev  <- evidenceProject rn proj (map fst inBoth)
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _err ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])
