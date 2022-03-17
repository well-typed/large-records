{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.Isomorphic (
    CIsomorphic(..)
  , parseIsomorphic
  , solveIsomorphic
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

-- | Parsed form of an @Isomorphic r r'@ constraint
data CIsomorphic = CIsomorphic {
      -- | Fields on the LHS
      isomorphicParsedLHS :: Fields

      -- | Fields on the RHS
    , isomorphicParsedRHS :: Fields

      -- | Left-hand side of the isomorphism (@r@)
    , isomorphicTypeLHS :: Type

      -- | Right-hand side of the isomorphism (@r'@)
    , isomorphicTypeRHS :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CIsomorphic where
  ppr (CIsomorphic parsedLHS parsedRHS typeLHS typeRHS) = parens $
      text "CIsomorphic" <+> braces (vcat [
          text "isomorphicParsedLHS" <+> text "=" <+> ppr parsedLHS
        , text "isomorphicParsedRHS" <+> text "=" <+> ppr parsedRHS
        , text "isomorphicTypeLHS"   <+> text "=" <+> ppr typeLHS
        , text "isomorphicTypeRHS"   <+> text "=" <+> ppr typeRHS
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseIsomorphic ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CIsomorphic)
parseIsomorphic tcs rn@ResolvedNames{..} =
    parseConstraint' clsIsomorphic $ \[typeLHS, typeRHS] -> do
      fieldsLHS <- parseFields tcs rn typeLHS
      fieldsRHS <- parseFields tcs rn typeRHS
      return $ CIsomorphic {
            isomorphicParsedLHS = fieldsLHS
          , isomorphicParsedRHS = fieldsRHS
          , isomorphicTypeLHS   = typeLHS
          , isomorphicTypeRHS   = typeRHS
          }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceIsomorphic ::
     ResolvedNames
  -> CIsomorphic
  -> [Int]
  -> TcPluginM 'Solve EvTerm
evidenceIsomorphic ResolvedNames{..} CIsomorphic{..} fields = do
    return $
      evDataConApp
        (classDataCon clsIsomorphic)
        [isomorphicTypeLHS, isomorphicTypeRHS]
        [ mkCoreApps (Var idEvidenceIsomorphic) [
              Type isomorphicTypeLHS
            , Type isomorphicTypeRHS
            , mkListExpr intTy $ map (mkUncheckedIntExpr . fromIntegral) fields
            ]
        ]

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveIsomorphic ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CIsomorphic
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveIsomorphic rn orig (L loc iso@CIsomorphic{..}) =
    case ( checkAllFieldsKnown isomorphicParsedLHS
         , checkAllFieldsKnown isomorphicParsedRHS
         ) of
      (Just lhs, Just rhs) ->
        case knownRecordIsomorphic lhs rhs of
          ([], inBoth) -> do
            eqs <- forM inBoth $ \(_n, (l, r)) ->
                     newWanted loc $
                       mkPrimEqPredRole
                         Nominal
                         (knownFieldType l)
                         (knownFieldType r)
            ev  <- evidenceIsomorphic rn iso (mkPerm lhs rhs)
            return (Just (ev, orig), map mkNonCanonical eqs)
          _otherwise ->
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
    inOld KnownField{..} = knownRecordIndices old HashMap.! knownFieldName

