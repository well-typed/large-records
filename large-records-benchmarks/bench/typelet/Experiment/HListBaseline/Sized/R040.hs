#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.HListBaseline.Sized.R040 where

import Bench.Types
import Bench.HList
import Common.HListOfSize.HL040

hlist :: HList Fields
hlist =
       -- 00 .. 09
       MkT 00
    :* MkT 01
    :* MkT 02
    :* MkT 03
    :* MkT 04
    :* MkT 05
    :* MkT 06
    :* MkT 07
    :* MkT 08
    :* MkT 09
       -- 10 .. 19
    :* MkT 10
    :* MkT 11
    :* MkT 12
    :* MkT 13
    :* MkT 14
    :* MkT 15
    :* MkT 16
    :* MkT 17
    :* MkT 18
    :* MkT 19
       -- 20 .. 29
    :* MkT 20
    :* MkT 21
    :* MkT 22
    :* MkT 23
    :* MkT 24
    :* MkT 25
    :* MkT 26
    :* MkT 27
    :* MkT 28
    :* MkT 29
       -- 30 .. 39
    :* MkT 30
    :* MkT 31
    :* MkT 32
    :* MkT 33
    :* MkT 34
    :* MkT 35
    :* MkT 36
    :* MkT 37
    :* MkT 38
    :* MkT 39
    :* Nil
