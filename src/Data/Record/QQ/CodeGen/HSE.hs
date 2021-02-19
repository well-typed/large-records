{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional utilities for working with @haskell-src-exts@
module Data.Record.QQ.CodeGen.HSE (
    extensionFromTH
  , processRecordPuns
  ) where

import Data.Generics
import Language.Haskell.Exts

import qualified Language.Haskell.TH as TH

-- | Translate TH extension into HSE extension
--
-- Useful in combination with 'extsEnabled'.
extensionFromTH :: TH.Extension -> Extension
extensionFromTH = \case
    TH.RecordPuns -> EnableExtension $ NamedFieldPuns

    -- We don't care about all extensions; there are many of them, and they vary
    -- from ghc version to ghc version. Treating them all would be a lot of work
    -- for little benefit. We assume that calling @show@ gives us a valid
    -- extension name; by and large this seems to be true (though for instance
    -- it will give us 'RecordPuns' rather than 'NamedFieldPuns', which although
    -- valid, is deprecated).
    e -> UnknownExtension $ show e

processRecordPuns :: forall l. Data l => Pat l -> Pat l
processRecordPuns = everywhere (mkT go)
  where
    go :: PatField l -> PatField l
    go (PFieldPun pLoc n@(UnQual nLoc n')) = PFieldPat pLoc n (PVar nLoc n')
    go p = p
