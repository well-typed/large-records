{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Thin compatibility layer around TH
module Bench.TH.Compat (
    TyVarBndr
  , pattern PlainTV
  , pattern KindedTV
  ) where

import Language.Haskell.TH hiding (TyVarBndr(..), forallT, forallC)
import qualified Language.Haskell.TH as TH

#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr = TH.TyVarBndr ()
#else
type TyVarBndr = TH.TyVarBndr
#endif

pattern PlainTV  :: Name -> TyVarBndr
pattern KindedTV :: Name -> Kind -> TyVarBndr

#if MIN_VERSION_template_haskell(2,17,0)
pattern PlainTV  n   = TH.PlainTV  n ()
pattern KindedTV n k = TH.KindedTV n () k
#else
pattern PlainTV  n   = TH.PlainTV  n
pattern KindedTV n k = TH.KindedTV n k
#endif

{-# COMPLETE PlainTV, KindedTV #-}

