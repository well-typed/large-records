{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiment.ConstraintFamily_Deep (CF, withCF) where

import Data.Kind
import Data.Proxy

import Bench.HList
import Common.EmptyClass_Tree_Phantom
import Infra.Tree

type family CF (a :: Type) :: Constraint

withCF :: CF a => Proxy a -> ()
withCF _ = ()

-- | In the "deep" evaluation, we do not use an intermediate class
--
-- (This is probably easier to understand when compared to the shallow version.)
type instance CF (HList xs) = EmptyClass (ToTree xs)



