{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anonymous.Plugin.Constraints.Isomorphic (
    CIsomorphic(..)
  , parseIsomorphic
  , solveIsomorphic
  ) where

import Control.Monad (forM)
import Data.Void

import Data.Record.Anonymous.Plugin.GhcTcPluginAPI
import Data.Record.Anonymous.Plugin.NameResolution
import Data.Record.Anonymous.Plugin.Parsing
import Data.Record.Anonymous.Plugin.Record
import Data.Record.Anonymous.Plugin.TyConSubst
import qualified Data.Map as Map

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
    case ( checkAllFieldsKnown isomorphicFieldsLHS
         , checkAllFieldsKnown isomorphicFieldsRHS
         ) of
      (Just lhs, Just rhs) ->
        case knownRecordIsomorphic lhs rhs of
          (inBoth, [], []) -> do
            eqs <- forM (Map.elems inBoth) $ \((l, r), _info) ->
                     newWanted loc $ mkPrimEqPredRole Nominal l r
            ev  <- evidenceIsomorphic rn iso
            return (Just (ev, orig), map mkNonCanonical eqs)
          _otherwise ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])

