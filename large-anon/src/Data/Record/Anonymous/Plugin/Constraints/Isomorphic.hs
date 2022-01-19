{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.Isomorphic (
    CIsomorphic(..)
  , parseIsomorphic
  , solveIsomorphic
  ) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Void

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
      isomorphicFieldsLHS :: Fields

      -- | Fields on the RHS
    , isomorphicFieldsRHS :: Fields

      -- | Left-hand side of the isomorphism (@r@)
    , isomorphicTypeLHS :: Type

      -- | Right-hand side of the isomorphism (@r'@)
    , isomorphicTypeRHS :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CIsomorphic where
  ppr (CIsomorphic fieldsLHS fieldsRHS typeLHS typeRHS) = parens $
          text "CIsomorphic"
      <+> ppr fieldsLHS
      <+> ppr fieldsRHS
      <+> ppr typeLHS
      <+> ppr typeRHS

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
            isomorphicFieldsLHS = fieldsLHS
          , isomorphicFieldsRHS = fieldsRHS
          , isomorphicTypeLHS   = typeLHS
          , isomorphicTypeRHS   = typeRHS
          }

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceIsomorphic :: ResolvedNames -> CIsomorphic -> TcPluginM 'Solve EvTerm
evidenceIsomorphic ResolvedNames{..} CIsomorphic{..} = do
     return $
       evDataConApp
         (classDataCon clsIsomorphic)
         [isomorphicTypeLHS, isomorphicTypeRHS]
         []

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveIsomorphic ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CIsomorphic
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveIsomorphic rn orig (L loc iso@CIsomorphic{..}) =
    case ( allFieldsKnown isomorphicFieldsLHS
         , allFieldsKnown isomorphicFieldsRHS
         ) of
      (Just (KnownRecord lhs), Just (KnownRecord rhs)) ->
        -- TODO: Think carefully about duplicated fields
        -- Can (only?) arise here as a result of a merge..?
        case matchKnownFields lhs rhs of
          Right match -> do
            eqs <- forM match $ \(l, r) ->
                     newWanted loc $ mkPrimEqPredRole Nominal l r
            ev <- evidenceIsomorphic rn iso
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _notIso ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])

{-------------------------------------------------------------------------------
  Internal: the actual isomorphism check
-------------------------------------------------------------------------------}

-- | Evidence that two records are not isomorphic
--
-- Invariant:
--
-- > (not . null) (notIsoMissingFromRight ++ notIsoMissingFromLeft)
data NotIso = NotIso {
      -- | Fields that are in the LHS but not in the RHS
      notIsoMissingFromRight :: [FastString]

      -- | Fields that are in the RHS but not in the LHS
    , notIsoMissingFromLeft  :: [FastString]
    }

-- | Check if known records have the same fields
--
-- If so, returns the types of those fields that must be pairwise equal.
matchKnownFields ::
     [(FastString, KnownField a)]
  -> [(FastString, KnownField a)]
  -> Either NotIso [(Type, Type)]
matchKnownFields = \lhs rhs ->
    go [] [] [] (sortOn fst lhs) (sortOn fst rhs)
  where
    go :: [FastString]   -- Accumulator: missing from right
       -> [FastString]   -- Accumulator: missing from left
       -> [(Type, Type)] -- Accumulator: fields that should be equal
       -> [(FastString, KnownField a)]
       -> [(FastString, KnownField a)]
       -> Either NotIso [(Type, Type)]
    go missR missL match lhs rhs =
        case (lhs, rhs) of
          ([], []) -> finishWith              missR               missL  match
          (ls, []) -> finishWith (names ls ++ missR)              missL  match
          ([], rs) -> finishWith              missR  (names rs ++ missL) match
          (l@(n, KnownField t _):ls, r@(n', KnownField t' _):rs)
            | n < n'    -> go (n:missR)     missL          match     ls (r:rs)
            | n > n'    -> go    missR  (n':missL)         match  (l:ls)   rs
            | otherwise -> go    missR      missL  ((t,t'):match)    ls    rs

    finishWith ::
         [FastString]
      -> [FastString]
      -> [(Type, Type)]
      -> Either NotIso [(Type, Type)]
    finishWith []        []       match = Right match
    finishWith missR missL _     = Left (NotIso missL missR)

    names :: [(FastString, KnownField a)] -> [FastString]
    names = map fst