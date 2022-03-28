#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# OPTIONS_GHC -fplugin=Data.Record.Anonymous.Plugin #-}

module Experiment.ConstructNoTypeLet.Sized.R080 where

import Data.Record.Anonymous.Simple (Record)

import Bench.Types
import Common.RowOfSize.Row080

record :: Record Row
record = ANON {
      -- 00 .. 09
      t00 = MkT 00
    , t01 = MkT 01
    , t02 = MkT 02
    , t03 = MkT 03
    , t04 = MkT 04
    , t05 = MkT 05
    , t06 = MkT 06
    , t07 = MkT 07
    , t08 = MkT 08
    , t09 = MkT 09
      -- 10 .. 19
    , t10 = MkT 10
    , t11 = MkT 11
    , t12 = MkT 12
    , t13 = MkT 13
    , t14 = MkT 14
    , t15 = MkT 15
    , t16 = MkT 16
    , t17 = MkT 17
    , t18 = MkT 18
    , t19 = MkT 19
      -- 20 .. 29
    , t20 = MkT 20
    , t21 = MkT 21
    , t22 = MkT 22
    , t23 = MkT 23
    , t24 = MkT 24
    , t25 = MkT 25
    , t26 = MkT 26
    , t27 = MkT 27
    , t28 = MkT 28
    , t29 = MkT 29
      -- 30 .. 39
    , t30 = MkT 30
    , t31 = MkT 31
    , t32 = MkT 32
    , t33 = MkT 33
    , t34 = MkT 34
    , t35 = MkT 35
    , t36 = MkT 36
    , t37 = MkT 37
    , t38 = MkT 38
    , t39 = MkT 39
      -- 40 .. 49
    , t40 = MkT 40
    , t41 = MkT 41
    , t42 = MkT 42
    , t43 = MkT 43
    , t44 = MkT 44
    , t45 = MkT 45
    , t46 = MkT 46
    , t47 = MkT 47
    , t48 = MkT 48
    , t49 = MkT 49
      -- 50 .. 59
    , t50 = MkT 50
    , t51 = MkT 51
    , t52 = MkT 52
    , t53 = MkT 53
    , t54 = MkT 54
    , t55 = MkT 55
    , t56 = MkT 56
    , t57 = MkT 57
    , t58 = MkT 58
    , t59 = MkT 59
      -- 60 .. 69
    , t60 = MkT 60
    , t61 = MkT 61
    , t62 = MkT 62
    , t63 = MkT 63
    , t64 = MkT 64
    , t65 = MkT 65
    , t66 = MkT 66
    , t67 = MkT 67
    , t68 = MkT 68
    , t69 = MkT 69
      -- 70 .. 79
    , t70 = MkT 70
    , t71 = MkT 71
    , t72 = MkT 72
    , t73 = MkT 73
    , t74 = MkT 74
    , t75 = MkT 75
    , t76 = MkT 76
    , t77 = MkT 77
    , t78 = MkT 78
    , t79 = MkT 79
    }