{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Convert GHC AST definitions of records into our own representation, 'Record'.
module Data.Record.Plugin.Types.Record where

import Control.Monad.Except
import Data.Record.Plugin.GHC
import Data.Record.Plugin.Types.Exception (Exception (..))
import Data.Record.Plugin.Types.Options (LargeRecordOptions)
import Data.Traversable (for)

-- | A representation for records that can be processed by large-records.
data Record = Record
  { tyName :: RdrName,
    tyVars :: [HsTyVarBndr GhcPs],
    conName :: RdrName,
    fields :: [(RdrName, HsType GhcPs)],
    derivings :: [RecordDeriving],
    options :: LargeRecordOptions
  }

-- | Derived classes that we can support.
data StockDeriving = Eq | Show | Ord | Generic

-- | A representation for @deriving@ clauses.
data RecordDeriving
  = DeriveStock StockDeriving
  | DeriveAnyClass (LHsType GhcPs)

-- Views

viewDataDeclName :: LHsDecl GhcPs -> Maybe RdrName
viewDataDeclName = \case
  L _ (TyClD _ DataDecl {tcdLName = L _ tyName}) -> Just tyName
  _ -> Nothing

viewRecord :: MonadError Exception m => LargeRecordOptions -> LHsDecl GhcPs -> m Record
viewRecord options (L _ decl) = case decl of
  TyClD
    _
    DataDecl
      { tcdLName = L _ tyName,
        tcdTyVars = HsQTvs {hsq_explicit = tyVars},
        tcdFixity = Prefix,
        tcdDataDefn =
          HsDataDefn
            { dd_ND = DataType,
              dd_ctxt = L _ [],
              dd_cType = Nothing,
              dd_kindSig = Nothing,
              dd_cons =
                [ L
                    _
                    ConDeclH98
                      { con_name = L _ conName,
                        con_forall = L _ False,
                        con_ex_tvs = [],
                        con_mb_cxt = Nothing,
                        con_args = RecCon (L _ fields)
                      }
                  ],
              dd_derivs = L _ derivs
            }
      } -> do
      derivings <- viewRecordDerivings derivs
      pure
        Record
          { tyName,
            tyVars = unLoc <$> tyVars,
            conName,
            fields = [p | f <- fields, p <- viewFieldsPairs f],
            derivings,
            options
          }
  _ -> throwError InvalidDeclaration

viewFieldsPairs :: LConDeclField GhcPs -> [(RdrName, HsType GhcPs)]
viewFieldsPairs = \case
  L _ ConDeclField {cd_fld_names, cd_fld_type = L _ ty} ->
    [(name, ty) | L _ FieldOcc {rdrNameFieldOcc = L _ name} <- cd_fld_names]
  _ -> []

viewRecordDerivings :: MonadError Exception m => [LHsDerivingClause GhcPs] -> m [RecordDeriving]
viewRecordDerivings = fmap concat . traverse viewRecordDeriving

viewRecordDeriving :: MonadError Exception m => LHsDerivingClause GhcPs -> m [RecordDeriving]
viewRecordDeriving = \case
  L _ HsDerivingClause {deriv_clause_strategy, deriv_clause_tys} ->
    case deriv_clause_strategy of
      Nothing -> throwError DerivingWithoutStrategy
      Just (L _ AnyclassStrategy) ->
        pure [DeriveAnyClass c | HsIB _ c <- unLoc deriv_clause_tys]
      Just (L _ StockStrategy) ->
        for (hsib_body <$> unLoc deriv_clause_tys) \case
          L _ (HsTyVar _ _ (L _ (rdrNameString -> "Show"))) -> pure (DeriveStock Show)
          L _ (HsTyVar _ _ (L _ (rdrNameString -> "Eq"))) -> pure (DeriveStock Eq)
          L _ (HsTyVar _ _ (L _ (rdrNameString -> "Ord"))) -> pure (DeriveStock Ord)
          L _ (HsTyVar _ _ (L _ (rdrNameString -> "Generic"))) -> pure (DeriveStock Generic)
          L _ ty -> throwError (UnsupportedStockDeriving ty)
      Just (L _ strategy) -> throwError (UnsupportedStrategy strategy)
  _ -> pure []

