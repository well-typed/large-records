#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.ApBaseline.Sized.R060 where

import Bench.Types
import Common.FunOfArity.F060

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
        -- 30 .. 39
    <*> pure (MkT 30)
    <*> pure (MkT 31)
    <*> pure (MkT 32)
    <*> pure (MkT 33)
    <*> pure (MkT 34)
    <*> pure (MkT 35)
    <*> pure (MkT 36)
    <*> pure (MkT 37)
    <*> pure (MkT 38)
    <*> pure (MkT 39)
        -- 40 .. 49
    <*> pure (MkT 40)
    <*> pure (MkT 41)
    <*> pure (MkT 42)
    <*> pure (MkT 43)
    <*> pure (MkT 44)
    <*> pure (MkT 45)
    <*> pure (MkT 46)
    <*> pure (MkT 47)
    <*> pure (MkT 48)
    <*> pure (MkT 49)
        -- 50 .. 59
    <*> pure (MkT 50)
    <*> pure (MkT 51)
    <*> pure (MkT 52)
    <*> pure (MkT 53)
    <*> pure (MkT 54)
    <*> pure (MkT 55)
    <*> pure (MkT 56)
    <*> pure (MkT 57)
    <*> pure (MkT 58)
    <*> pure (MkT 59)
