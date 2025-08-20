{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}

module Common.EmptyClass_Tree_Nominal (
    EmptyClass
  , requireEmptyClass
  , requireEmptyClass_preEval
  ) where

import Data.Kind
import Data.Proxy

import Infra.Tree

class EmptyClass (xs :: Tree Type) where

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

