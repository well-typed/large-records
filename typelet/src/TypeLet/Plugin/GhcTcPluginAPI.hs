{-# LANGUAGE CPP #-}

-- | Thin layer around ghc-tcplugin-api
module TypeLet.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Utils.Outputable

    -- * Additional re-exports
    -- ** Substitutions
  , Subst -- opaque
  , Subst.substTy
  , Subst.substTyWith
  , Subst.zipTvSubst
  , elemVarSet
  , tyCoVarsOfType
  ) where

import GHC.TcPlugin.API
import GHC.Utils.Outputable

#if MIN_VERSION_ghc(9,0,0)

import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Types.Var.Set (elemVarSet)

import qualified GHC.Core.TyCo.Subst as Subst

#else

import VarSet (elemVarSet)
import TcType (tyCoVarsOfType)

import qualified TcType as Subst

#endif

{-------------------------------------------------------------------------------
  TCvSubst was renamed to Subst in ghc 9.6
-------------------------------------------------------------------------------}

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCo.Subst (Subst)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Core.TyCo.Subst (TCvSubst)
#else
import TcType (TCvSubst)
#endif

#if !MIN_VERSION_ghc(9,6,0)
type Subst = TCvSubst
#endif





