{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiment.ConstraintFamily_Shallow (CF, withCF) where

import Data.Kind
import Data.Proxy

import Bench.HList
import Common.EmptyClass_Tree_Phantom
import Infra.Tree

type family CF (a :: Type) :: Constraint

withCF :: CF a => Proxy a -> ()
withCF _ = ()

class    EmptyClass (ToTree xs) => CF_HList xs
instance EmptyClass (ToTree xs) => CF_HList xs

-- | In the " shallow " evaluation, we introduce an intermediate class.
type instance CF (HList xs) = CF_HList xs

