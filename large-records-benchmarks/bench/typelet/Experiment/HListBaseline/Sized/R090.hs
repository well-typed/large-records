#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

module Experiment.HListBaseline.Sized.R090 where

import Bench.Types
import Bench.HList
import Common.HListOfSize.HL090

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
       -- 40 .. 49
    :* MkT 40
    :* MkT 41
    :* MkT 42
    :* MkT 43
    :* MkT 44
    :* MkT 45
    :* MkT 46
    :* MkT 47
    :* MkT 48
    :* MkT 49
       -- 50 .. 59
    :* MkT 50
    :* MkT 51
    :* MkT 52
    :* MkT 53
    :* MkT 54
    :* MkT 55
    :* MkT 56
    :* MkT 57
    :* MkT 58
    :* MkT 59
       -- 60 .. 69
    :* MkT 60
    :* MkT 61
    :* MkT 62
    :* MkT 63
    :* MkT 64
    :* MkT 65
    :* MkT 66
    :* MkT 67
    :* MkT 68
    :* MkT 69
       -- 70 .. 79
    :* MkT 70
    :* MkT 71
    :* MkT 72
    :* MkT 73
    :* MkT 74
    :* MkT 75
    :* MkT 76
    :* MkT 77
    :* MkT 78
    :* MkT 79
       -- 80 .. 89
    :* MkT 80
    :* MkT 81
    :* MkT 82
    :* MkT 83
    :* MkT 84
    :* MkT 85
    :* MkT 86
    :* MkT 87
    :* MkT 88
    :* MkT 89
    :* Nil
