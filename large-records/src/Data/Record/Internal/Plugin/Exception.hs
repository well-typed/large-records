{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exceptions that may be thrown by the large-records plugin.
module Data.Record.Internal.Plugin.Exception (
    Exception(..)
  , formatException
  ) where

import Data.Record.Internal.GHC.Shim

data Exception =
    UnsupportedStockDeriving (LHsType GhcPs)
  | UnsupportedStrategy (DerivStrategy GhcPs)
  | InvalidDeclaration

-- TODO: better exception messages
formatException :: Exception -> SDoc
formatException = hsep . \case
    UnsupportedStockDeriving ty -> [
        "Unsupported stock class: "
      , ppr ty
      ]
    UnsupportedStrategy strategy -> [
        "Strategy "
      , derivStrategyName strategy
      , " is not supported"
      ]
    InvalidDeclaration -> [
        "Unsupported declaration for large-records"
      ]
