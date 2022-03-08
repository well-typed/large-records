{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Convert GHC AST definitions of records into our own representation, 'Record'.
module Data.Record.Plugin.Types.Record (
    Record(..)
  , StockDeriving(..)
  , RecordDeriving(..)
  , viewRecord
  ) where

import Control.Monad.Except
import Data.Traversable (for)

import Data.Record.Plugin.GHC.Shim
import Data.Record.Plugin.GHC.TemplateHaskellStyle
import Data.Record.Plugin.Types.Exception (Exception (..))
import Data.Record.Plugin.Types.Options (LargeRecordOptions)

-- | A representation for records that can be processed by large-records.
--
-- TODO: We should change this representation to keep as much location info
-- as we can, for better error messages. (Nearly?) all use of 'noLoc' should go.
data Record = Record
  { tyName :: RdrName,
    tyVars :: [LHsTyVarBndr GhcPs],
    conName :: RdrName,
    fields :: [(RdrName, LHsType GhcPs)],
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

viewRecord :: MonadError Exception m => LargeRecordOptions -> LHsDecl GhcPs -> m Record
viewRecord options decl =
    case decl of
      DataD tyName tyVars [RecC conName fields] derivs-> do
        derivings <- viewRecordDerivings derivs
        pure Record {
            tyVars
          , fields
          , conName
          , tyName
          , derivings
          , options
          }
      _otherwise -> throwError InvalidDeclaration

viewRecordDerivings :: MonadError Exception m => [LHsDerivingClause GhcPs] -> m [RecordDeriving]
viewRecordDerivings = fmap concat . traverse viewRecordDeriving

viewRecordDeriving :: MonadError Exception m => LHsDerivingClause GhcPs -> m [RecordDeriving]
viewRecordDeriving = \case
    DerivClause Nothing _ ->
      throwError DerivingWithoutStrategy
    DerivClause (Just (L _ AnyclassStrategy)) tys ->
      pure $ map DeriveAnyClass tys
    DerivClause (Just (L _ StockStrategy)) tys ->
      for tys $ \case
        VarT (nameBase -> "Show")    -> pure $ DeriveStock Show
        VarT (nameBase -> "Eq")      -> pure $ DeriveStock Eq
        VarT (nameBase -> "Ord")     -> pure $ DeriveStock Ord
        VarT (nameBase -> "Generic") -> pure $ DeriveStock Generic
        ty -> throwError (UnsupportedStockDeriving ty)
    DerivClause (Just (L _ strategy)) _ ->
      throwError (UnsupportedStrategy strategy)
    _ ->
      pure []
