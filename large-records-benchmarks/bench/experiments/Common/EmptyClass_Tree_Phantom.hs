{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RoleAnnotations     #-}

module Common.EmptyClass_Tree_Phantom (
    EmptyClass
  , requireEmptyClass
  , requireEmptyClass_preEval
  ) where

import Data.Kind
import Data.Proxy

import Infra.Tree

class EmptyClass (xs :: Tree Type) where
type role EmptyClass phantom

instance EmptyClass 'Zero
instance EmptyClass ('One x)
instance EmptyClass ('Two x1 x2)
instance (EmptyClass l, EmptyClass r) => EmptyClass ('Branch x l r)

{-# NOINLINE requireEmptyClass #-}
requireEmptyClass :: EmptyClass (ToTree xs) => Proxy xs -> ()
requireEmptyClass _ = ()

{-# NOINLINE requireEmptyClass_preEval #-}
requireEmptyClass_preEval :: EmptyClass (ToTree_PreEval xs) => Proxy xs -> ()
requireEmptyClass_preEval _ = ()

