{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.Project (
    CProject(..)
  , parseProject
  , solveProject
  ) where

import Control.Monad (forM)
import Data.Void

import qualified Data.HashMap.Strict as HashMap

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of an @Project (r :: [(Symbol, k)]) (r' :: [(Symbol, k)])@ constraint
data CProject = CProject {
      -- | Fields on the LHS
      projectParsedLHS :: Fields

      -- | Fields on the RHS
    , projectParsedRHS :: Fields

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
  ppr (CProject parsedLHS parsedRHS typeLHS typeRHS typeKind) = parens $
      text "CProject" <+> braces (vcat [
          text "projectParsedLHS" <+> text "=" <+> ppr parsedLHS
        , text "projectParsedRHS" <+> text "=" <+> ppr parsedRHS
        , text "projectTypeLHS"   <+> text "=" <+> ppr typeLHS
        , text "projectTypeRHS"   <+> text "=" <+> ppr typeRHS
        , text "projectTypeKind"  <+> text "=" <+> ppr typeKind
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
    parseConstraint' clsProject $ \[typeKind, typeLHS, typeRHS] -> do
      fieldsLHS <- parseFields tcs rn typeLHS
      fieldsRHS <- parseFields tcs rn typeRHS
      return $ CProject {
            projectParsedLHS = fieldsLHS
          , projectParsedRHS = fieldsRHS
          , projectTypeLHS   = typeLHS
          , projectTypeRHS   = typeRHS
          , projectTypeKind  = typeKind
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
    case ( checkAllFieldsKnown projectParsedLHS
         , checkAllFieldsKnown projectParsedRHS
         ) of
      (Just lhs, Just rhs) ->
        case checkCanProject lhs rhs of
          Right inBoth -> do
            eqs <- forM inBoth $ \(l, r) ->
                     newWanted loc $
                       mkPrimEqPredRole
                         Nominal
                         (knownFieldType l)
                         (knownFieldType r)
            ev  <- evidenceProject rn proj (mkPerm lhs rhs)
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _err ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])

-- | Construct permutation
--
-- Precondition: the two records are in fact isomorphic.
mkPerm :: KnownRecord a -> KnownRecord b -> [Int]
mkPerm old new =
    map inOld (knownRecordFields new)
  where
    inOld :: KnownField b -> Int
    inOld KnownField{..} = knownRecordVisible old HashMap.! knownFieldName

