{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}

#ifdef BLOG2_VARIANT_LOGARITHMIC
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RoleAnnotations     #-}
#endif

-- {-# OPTIONS_GHC -ddump-ds-preopt #-}

module Test.Record.Experiments.Induction.Tree (
    EmptyClass
  , requireEmptyClass
  ) where

import Data.Kind
import Data.Proxy

import Test.Record.Experiments.Util

class EmptyClass (xs :: Tree Type) where

#ifdef BLOG2_VARIANT_LOGARITHMIC
type role EmptyClass phantom
#endif

instance EmptyClass 'Zero
instance EmptyClass ('One x)
instance EmptyClass ('Two x1 x2)
instance (EmptyClass l, EmptyClass r) => EmptyClass ('Branch x l r)

{-# NOINLINE requireEmptyClass #-}
requireEmptyClass :: EmptyClass (ToTree xs) => Proxy xs -> ()
requireEmptyClass _ = ()


