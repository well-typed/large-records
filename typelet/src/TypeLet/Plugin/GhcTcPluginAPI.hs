{-# LANGUAGE CPP #-}

-- | Thin layer around ghc-tcplugin-api
module TypeLet.Plugin.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Utils.Outputable

    -- * Additional re-exports
    -- ** Substitutions
  , TCvSubst
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
import TcType (TCvSubst, tyCoVarsOfType)

import qualified TcType as Subst

#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Core.Subst (Subst)
type TCvSubst = Subst
#else
import GHC.Core.TyCo.Subst (TCvSubst)
#endif
