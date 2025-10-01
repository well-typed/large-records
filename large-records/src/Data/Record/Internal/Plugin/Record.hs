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
      recordTyName    :: LIdP GhcPs
    , recordTyVars    :: [LHsTyVarBndr GhcPs]
    , recordConName   :: LIdP GhcPs
    , recordFields    :: [Field]
    , recordDerivings :: [RecordDeriving]
    , recordOptions   :: LargeRecordOptions

      -- | The location of the @ANN@ pragma
      --
      -- We use this as the location of the new identifiers we generate.
    , recordAnnLoc    :: SrcSpan
    }

data Field = Field {
      fieldName       :: LIdP GhcPs
    , fieldType       :: LHsType GhcPs
    , fieldStrictness :: HsSrcBang
    , fieldIndex      :: Int
    }

-- | Derived classes that we can support.
data StockDeriving = Eq | Show | Ord | Generic

-- | A representation for @deriving@ clauses.
--
-- NOTE: We support @DeriveAnyClass@ style derivation, because this does not
-- depend on the internal representation we choose, but only on the default
-- implementation in the class, which typically depends on generics. For
-- example, it makes it possible to define things like
--
-- > data UserT (f :: Type -> Type) = User {
-- >       userEmail :: Columnar f Text
-- >       -- .. other fields ..
-- >     }
-- >   deriving stock (Show, Eq)
-- >   deriving anyclass (Beamable)
--
-- For now we do /not/ support newtype deriving or deriving-via, since this
-- /does/ depend on the internal record representation. See discussion at
-- <https://github.com/well-typed/large-records/pull/42>.
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
  => (LIdP GhcPs, LHsType GhcPs, HsSrcBang) -> m (Int -> Field)
viewField (name, typ, bang) =
  return $ Field name (parensT typ) bang

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
    DerivClause (Just (L _ StockStrategy {})) tys ->
      goStock tys
    DerivClause (Just (L _ AnyclassStrategy {})) tys ->
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
