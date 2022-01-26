{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

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

    -- * Additional exports
  , splitAppTys

    -- * New functonality
  , isCanonicalVarEq
  ) where

#if __GLASGOW_HASKELL__ < 900
import Data.List.NonEmpty (NonEmpty, toList)
#endif

import GHC.TcPlugin.API
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.Make
import GHC.Utils.Outputable

#if __GLASGOW_HASKELL__ >= 808 &&  __GLASGOW_HASKELL__ < 810
import TcRnTypes (Ct(..))
import Type (splitAppTys)
#endif

#if __GLASGOW_HASKELL__ >= 810 &&  __GLASGOW_HASKELL__ < 900
import Constraint (Ct(..))
import Type (splitAppTys)
#endif

#if __GLASGOW_HASKELL__ >= 900 &&  __GLASGOW_HASKELL__ < 902
import GHC.Core.Type (splitAppTys)
import GHC.Tc.Types.Constraint (Ct(..))
#endif

#if __GLASGOW_HASKELL__ >= 902
import GHC.Core.Type (splitAppTys)
import GHC.Tc.Types.Constraint (Ct(..), CanEqLHS(..))
#endif

isCanonicalVarEq :: Ct -> Maybe (TcTyVar, Type)
#if __GLASGOW_HASKELL__ >= 808 &&  __GLASGOW_HASKELL__ < 902
isCanonicalVarEq = \case
    CTyEqCan{..}  -> Just (cc_tyvar, cc_rhs)
    CFunEqCan{..} -> Just (cc_fsk, mkTyConApp cc_fun cc_tyargs)
    _otherwise    -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 902
isCanonicalVarEq = \case
    CEqCan{..}
      | TyVarLHS var <- cc_lhs
      -> Just (var, cc_rhs)
      | TyFamLHS tyCon args <- cc_lhs
      , Just var            <- getTyVar_maybe cc_rhs
      -> Just (var, mkTyConApp tyCon args)
    _otherwise
      -> Nothing
#endif

-- TODO: Ideally we would actually show the location information obviously
instance Outputable CtLoc where
  ppr _ = text "<CtLoc>"

#if __GLASGOW_HASKELL__ < 900
instance Outputable a => Outputable (NonEmpty a) where
  ppr = ppr . toList
#endif

#if __GLASGOW_HASKELL__ >= 902
instance (Outputable l, Outputable e) => Outputable (GenLocated l e) where
  ppr (L l e) = parens $ text "L" <+> ppr l <+> ppr e
#endif