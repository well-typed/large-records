{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.Isomorphic (
    CIsomorphic(..)
  , parseIsomorphic
  , solveIsomorphic
  ) where

import Control.Monad (forM)
import Data.Void

import qualified Data.Map as Map

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
  -> [(FastString, Int)] -- ^ Matches 'Permutation'
  -> TcPluginM 'Solve EvTerm
evidenceIsomorphic ResolvedNames{..} CIsomorphic{..} perm = do
     fields <- mapM onField perm
     return $
       evDataConApp
         (classDataCon clsIsomorphic)
         [isomorphicTypeLHS, isomorphicTypeRHS]
         [ mkCoreApps (Var idEvidenceIsomorphic) [
               Type isomorphicTypeLHS
             , Type isomorphicTypeRHS
             , mkListExpr (mkTupleTy Boxed [stringTy, intTy]) fields
             ]
         ]
  where
    onField :: (FastString, Int) -> TcPluginM 'Solve CoreExpr
    onField (name, i) = do
        name' <- mkStringExprFS name
        return $ mkCoreTup [name', mkUncheckedIntExpr (fromIntegral i)]

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
          (inBoth, [], []) -> do
            eqs <- forM (Map.elems inBoth) $ \((l, r), _info) ->
                     newWanted loc $ mkPrimEqPredRole Nominal l r
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
mkPerm :: KnownRecord a -> KnownRecord b -> [(FastString, Int)]
mkPerm old new =
    map inOld (knownRecordFields new)
  where
    inOld :: KnownField b -> (FastString, Int)
    inOld KnownField{..} = (
          knownFieldName
        , knownRecordIndices old Map.! knownFieldName
        )

