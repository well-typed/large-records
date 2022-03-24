#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.ApBaseline.Sized.R030 where

import Bench.Types
import Common.FunOfArity.F030

applyF :: Applicative f => F r -> f r
applyF f =
        pure f
        -- 00 .. 09
    <*> pure (MkT 00)
    <*> pure (MkT 01)
    <*> pure (MkT 02)
    <*> pure (MkT 03)
    <*> pure (MkT 04)
    <*> pure (MkT 05)
    <*> pure (MkT 06)
    <*> pure (MkT 07)
    <*> pure (MkT 08)
    <*> pure (MkT 09)
        -- 10 .. 19
    <*> pure (MkT 10)
    <*> pure (MkT 11)
    <*> pure (MkT 12)
    <*> pure (MkT 13)
    <*> pure (MkT 14)
    <*> pure (MkT 15)
    <*> pure (MkT 16)
    <*> pure (MkT 17)
    <*> pure (MkT 18)
    <*> pure (MkT 19)
        -- 20 .. 29
    <*> pure (MkT 20)
    <*> pure (MkT 21)
    <*> pure (MkT 22)
    <*> pure (MkT 23)
    <*> pure (MkT 24)
    <*> pure (MkT 25)
    <*> pure (MkT 26)
    <*> pure (MkT 27)
    <*> pure (MkT 28)
    <*> pure (MkT 29)
