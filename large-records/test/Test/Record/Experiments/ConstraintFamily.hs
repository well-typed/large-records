{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Record.Experiments.ConstraintFamily where

import Data.Kind
import Data.Proxy

import Test.Record.Experiments.HList
import Test.Record.Experiments.Induction.Tree
import Test.Record.Experiments.Util

type family CF (a :: Type) :: Constraint

withCF :: CF a => Proxy a -> ()
withCF _ = ()

#ifdef BLOG2_VARIANT_QUADRATIC
type instance CF (HList xs) = EmptyClass (ToTree xs)
#else
class    EmptyClass (ToTree xs) => CF_HList xs
instance EmptyClass (ToTree xs) => CF_HList xs

type instance CF (HList xs) = CF_HList xs
#endif



