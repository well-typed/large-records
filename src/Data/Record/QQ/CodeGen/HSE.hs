{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Additional utilities for working with @haskell-src-exts@
module Data.Record.QQ.CodeGen.HSE (
    -- * Language extensions
    extensionFromTH
  , processRecordPuns
    -- * Naming
  , fromHseName
  , resolveHseName
  , resolveKnownHseName
  ) where

import Data.Generics
import Language.Haskell.Exts
import Language.Haskell.TH.Syntax (Quasi)

import qualified Language.Haskell.TH.Syntax as TH

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
  Naming
-------------------------------------------------------------------------------}

-- | HSE generated names are always dynamically bound
fromHseName :: TH.Name -> N.Name flavour 'N.Dynamic
fromHseName = N.fromTH'

-- | Resolve HSE generated name
--
-- As mentioned in 'fromHseName', HSE generated names are always dynamically
-- bound, and we therefore need to do a "renaming pass": we need to resolve the
-- name. However, the exact name we want to lookup might not be the name as it
-- appears in the QQ place; for if the user writes @MkR@, the name we actually
-- want to look up might be, say, @LR__MkR@.
resolveHseName :: (Quasi m, N.LookupName ns')
  => (String -> String)
  ->           N.Name ns  'N.Dynamic
  -> m (Maybe (N.Name ns' 'N.Global))
resolveHseName f = N.lookupName . N.mapNameBase f

-- | Variation on 'resolveHseName' that fails if the name is not known
resolveKnownHseName :: (Quasi m, N.LookupName ns')
  => (String -> String)
  ->    N.Name ns  'N.Dynamic
  -> m (N.Name ns' 'N.Global)
resolveKnownHseName f n = do
    mn' <- resolveHseName f n
    case mn' of
      Just n' -> return n'
      Nothing -> fail $ "resolveKnownHseName: " ++ N.nameBase n ++ " not in scope"