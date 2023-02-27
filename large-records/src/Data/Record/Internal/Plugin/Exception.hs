{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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
    UnsupportedStockDeriving x -> toSrcSpan x
    UnsupportedStrategy      x -> toSrcSpan x
    InvalidDeclaration       x -> toSrcSpan x

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
