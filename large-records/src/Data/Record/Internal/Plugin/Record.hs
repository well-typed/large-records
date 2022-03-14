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
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NE

import Data.Record.Internal.GHC.Shim
import Data.Record.Internal.GHC.TemplateHaskellStyle
import Data.Record.Internal.Plugin.Exception (Exception (..))
import Data.Record.Internal.Plugin.Options (LargeRecordOptions)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A representation for records that can be processed by large-records.
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
      _otherwise -> throwError $ InvalidDeclaration decl

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
      pure $ fmap DeriveAnyClass (NE.toList tys)
    DerivClause (Just strategy) _ ->
      throwError (UnsupportedStrategy strategy)
    _ ->
      pure []
  where
    goStock :: NonEmpty (LHsType GhcPs) -> m [RecordDeriving]
    goStock tys = for (NE.toList tys) $ \case
        ConT (nameBase -> "Show")    -> pure $ DeriveStock Show
        ConT (nameBase -> "Eq")      -> pure $ DeriveStock Eq
        ConT (nameBase -> "Ord")     -> pure $ DeriveStock Ord
        ConT (nameBase -> "Generic") -> pure $ DeriveStock Generic
        ty -> throwError (UnsupportedStockDeriving ty)
