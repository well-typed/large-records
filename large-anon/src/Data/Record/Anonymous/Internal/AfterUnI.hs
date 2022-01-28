{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Record.Anonymous.Internal.AfterUnI (
    UnI
  , AfterUnI(..)
  , wrapAfterUnI
  , unwrapAfterUnI
  ) where

import Data.SOP

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type family UnI a where
  UnI (I x) = x

newtype AfterUnI f x = AfterUnI (f (UnI x))

wrapAfterUnI :: f x -> AfterUnI f (I x)
wrapAfterUnI x = AfterUnI x

unwrapAfterUnI :: AfterUnI f (I x) -> f x
unwrapAfterUnI (AfterUnI x) = x

{-------------------------------------------------------------------------------
  "Proof of correctness by analogy": implement wrappers for NP
-------------------------------------------------------------------------------}

_wrapNpLeft :: SListI xs => NP f xs -> NP (I :.: f) xs
_wrapNpLeft = hmap (Comp . I)

_unwrapNpLeft :: SListI xs => NP (I :.: f) xs -> NP f xs
_unwrapNpLeft = hmap (unI . unComp)

_wrapNpRight :: SListI xs => NP f xs -> NP (AfterUnI f :.: I) xs
_wrapNpRight = hmap (Comp . wrapAfterUnI)

_unwrapNpRight :: SListI xs => NP (AfterUnI f :.: I) xs -> NP f xs
_unwrapNpRight = hmap (unwrapAfterUnI . unComp)
