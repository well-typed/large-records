{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exceptions that may be thrown by the large-records plugin.
module Data.Record.Internal.Plugin.Exception (
    Exception(..)
  , exceptionLoc
  , exceptionToSDoc
  ) where

import Data.Record.Internal.GHC.Shim

data Exception =
    UnsupportedStockDeriving (LHsType GhcPs)
  | UnsupportedStrategy (LDerivStrategy GhcPs)
  | InvalidDeclaration (LHsDecl GhcPs)

exceptionLoc :: Exception -> SrcSpan
exceptionLoc = \case
    UnsupportedStockDeriving (L l _) -> l
    UnsupportedStrategy      (L l _) -> l
    InvalidDeclaration       (L l _) -> l

exceptionToSDoc :: Exception -> SDoc
exceptionToSDoc = hsep . \case
    UnsupportedStockDeriving ty -> [
        "Unsupported stock class: "
      , ppr ty
      ]
    UnsupportedStrategy (L _ strategy) -> [
        "Strategy "
      , derivStrategyName strategy
      , " is not supported"
      ]
    InvalidDeclaration _decl -> [
        "Unsupported declaration for large-records"
      ]
