{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Thin layer around ghc-tcplugin-api
module Data.Record.Anonymous.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Builtin.Names
  , module GHC.Builtin.Types
  , module GHC.Builtin.Types.Prim
  , module GHC.Core.Make
  , module GHC.Utils.Outputable

    -- * New shims
  , newWanted'
  ) where

import GHC.TcPlugin.API
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.Make
import GHC.Utils.Outputable

-- Orphan instance, for debugging
instance Outputable CtLoc where
  ppr _ = text "<CtLoc>"

-- | Construct a new wanted constraint.
--
-- This works around a bug in GHC, making sure the source location is correct.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20895.
newWanted' :: CtLoc -> PredType -> TcPluginM 'Solve CtEvidence
newWanted' l w = setCtLocM l $ newWanted l w
