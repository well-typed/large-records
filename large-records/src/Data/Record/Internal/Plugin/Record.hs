{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Convert GHC AST definitions of records into our own representation, 'Record'.
module Data.Record.Internal.Plugin.Record (
    Record(..)
  , Field(..)
  , StockDeriving(..)
  , RecordDeriving(..)
  , viewRecord
  ) where

import Control.Monad.Except
import Data.Traversable (for)

import Data.Record.Internal.GHC.Shim
import Data.Record.Internal.GHC.TemplateHaskellStyle
import Data.Record.Internal.Plugin.Exception (Exception (..))
import Data.Record.Internal.Plugin.Options (LargeRecordOptions)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A representation for records that can be processed by large-records.
--
-- TODO: We should change this representation to keep as much location info
-- as we can, for better error messages. (Nearly?) all use of 'noLoc' should go.
data Record = Record {
      recordTyName    :: LRdrName
    , recordTyVars    :: [LHsTyVarBndr GhcPs]
    , recordConName   :: LRdrName
    , recordFields    :: [Field]
    , recordDerivings :: [RecordDeriving]
    , recordOptions   :: LargeRecordOptions

      -- | The location of the @ANN@ pragma
      --
      -- We use this as the location of the new identifiers we generate.
    , recordAnnLoc    :: SrcSpan
    }

data Field = Field {
      fieldName  :: LRdrName
    , fieldType  :: LHsType GhcPs
    , fieldIndex :: Int
    }

-- | Derived classes that we can support.
data StockDeriving = Eq | Show | Ord | Generic

-- | A representation for @deriving@ clauses.
data RecordDeriving =
    DeriveStock StockDeriving
  | DeriveAnyClass (LHsType GhcPs)

{-------------------------------------------------------------------------------
  Views
-------------------------------------------------------------------------------}

viewRecord ::
     MonadError Exception m
  => SrcSpan -> LargeRecordOptions -> LHsDecl GhcPs -> m Record
viewRecord annLoc options decl =
    case decl of
      DataD tyName tyVars [RecC conName fields] derivs-> do
        fields'   <- mapM viewField fields
        derivings <- viewRecordDerivings derivs
        pure Record {
            recordTyName    = tyName
          , recordTyVars    = tyVars
          , recordConName   = conName
          , recordFields    = zipWith ($) fields' [0..]
          , recordDerivings = derivings
          , recordOptions   = options
          , recordAnnLoc    = annLoc
          }
      _otherwise -> throwError InvalidDeclaration

viewField ::
     MonadError Exception m
  => (LRdrName, LHsType GhcPs) -> m (Int -> Field)
viewField (name, typ) = return $ Field name typ

viewRecordDerivings ::
     MonadError Exception m
  => [LHsDerivingClause GhcPs] -> m [RecordDeriving]
viewRecordDerivings = fmap concat . traverse viewRecordDeriving

viewRecordDeriving :: forall m.
     MonadError Exception m
  => LHsDerivingClause GhcPs -> m [RecordDeriving]
viewRecordDeriving = \case
    DerivClause Nothing tys ->
      goStock tys
    DerivClause (Just (L _ StockStrategy)) tys ->
      goStock tys
    -- TODO: Not sure that we want anyclass deriving
    -- See discussion in <https://github.com/well-typed/large-records/pull/42>.
    DerivClause (Just (L _ AnyclassStrategy)) tys ->
      pure $ map DeriveAnyClass tys
    DerivClause (Just (L _ strategy)) _ ->
      throwError (UnsupportedStrategy strategy)
    _ ->
      pure []
  where
    goStock :: [LHsType GhcPs] -> m [RecordDeriving]
    goStock tys = for tys $ \case
        VarT (nameBase -> "Show")    -> pure $ DeriveStock Show
        VarT (nameBase -> "Eq")      -> pure $ DeriveStock Eq
        VarT (nameBase -> "Ord")     -> pure $ DeriveStock Ord
        VarT (nameBase -> "Generic") -> pure $ DeriveStock Generic
        ty -> throwError (UnsupportedStockDeriving ty)

