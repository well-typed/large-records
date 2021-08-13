{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional utilities for working with @haskell-src-exts@
module Data.Record.QQ.CodeGen.HSE (
    -- * Language extensions
    extensionFromTH
  , processRecordPuns
    -- * Names
  , resolveConstr
  ) where

import Data.Generics
import Language.Haskell.Exts
import Language.Haskell.TH.Syntax (Quasi)

import qualified Language.Haskell.TH as TH

import qualified Data.Record.Internal.TH.Name as N

{-------------------------------------------------------------------------------
  Language extensions
-------------------------------------------------------------------------------}

-- | Translate TH extension into HSE extension
--
-- Useful in combination with 'extsEnabled'.
extensionFromTH :: TH.Extension -> Extension
extensionFromTH = \case
    TH.DataKinds        -> EnableExtension $ DataKinds
    TH.RecordPuns       -> EnableExtension $ NamedFieldPuns
    TH.TypeApplications -> EnableExtension $ TypeApplications
    TH.ViewPatterns     -> EnableExtension $ ViewPatterns
    TH.BlockArguments   -> EnableExtension $ BlockArguments

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

{-------------------------------------------------------------------------------
  Names

  Unlike a regular TH quotes, the names that appear in an expression of pattern
  parsed by @haskell-src-exts@ have not been properly renamed. We therefore have
  to do a lookup here.
-------------------------------------------------------------------------------}

-- | Resolve constructor name
--
-- The name we get from @haskell-src-meta@ will have a dynamic flavour (in
-- other words, will not have been through the renamer), and so we must resolve
-- it here.
resolveConstr :: Quasi m => TH.Name -> m (N.Name 'N.DataName 'N.Global)
resolveConstr n = do
    mConstr <- N.lookupName $ N.fromName' n
    case mConstr of
      Nothing      -> fail $ "resolveConstr: " ++ show n ++ " not in scope"
      Just constr' -> return constr'
