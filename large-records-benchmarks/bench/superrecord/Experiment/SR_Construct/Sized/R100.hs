#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Construct.Sized.R100 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row100 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      SR.rcons (#t00 := MkT x)
    $ SR.rcons (#t01 := MkT x)
    $ SR.rcons (#t02 := MkT x)
    $ SR.rcons (#t03 := MkT x)
    $ SR.rcons (#t04 := MkT x)
    $ SR.rcons (#t05 := MkT x)
    $ SR.rcons (#t06 := MkT x)
    $ SR.rcons (#t07 := MkT x)
    $ SR.rcons (#t08 := MkT x)
    $ SR.rcons (#t09 := MkT x)
      -- 10 .. 19
    $ SR.rcons (#t10 := MkT x)
    $ SR.rcons (#t11 := MkT x)
    $ SR.rcons (#t12 := MkT x)
    $ SR.rcons (#t13 := MkT x)
    $ SR.rcons (#t14 := MkT x)
    $ SR.rcons (#t15 := MkT x)
    $ SR.rcons (#t16 := MkT x)
    $ SR.rcons (#t17 := MkT x)
    $ SR.rcons (#t18 := MkT x)
    $ SR.rcons (#t19 := MkT x)
      -- 20 .. 29
    $ SR.rcons (#t20 := MkT x)
    $ SR.rcons (#t21 := MkT x)
    $ SR.rcons (#t22 := MkT x)
    $ SR.rcons (#t23 := MkT x)
    $ SR.rcons (#t24 := MkT x)
    $ SR.rcons (#t25 := MkT x)
    $ SR.rcons (#t26 := MkT x)
    $ SR.rcons (#t27 := MkT x)
    $ SR.rcons (#t28 := MkT x)
    $ SR.rcons (#t29 := MkT x)
      -- 30 .. 39
    $ SR.rcons (#t30 := MkT x)
    $ SR.rcons (#t31 := MkT x)
    $ SR.rcons (#t32 := MkT x)
    $ SR.rcons (#t33 := MkT x)
    $ SR.rcons (#t34 := MkT x)
    $ SR.rcons (#t35 := MkT x)
    $ SR.rcons (#t36 := MkT x)
    $ SR.rcons (#t37 := MkT x)
    $ SR.rcons (#t38 := MkT x)
    $ SR.rcons (#t39 := MkT x)
      -- 40 .. 49
    $ SR.rcons (#t40 := MkT x)
    $ SR.rcons (#t41 := MkT x)
    $ SR.rcons (#t42 := MkT x)
    $ SR.rcons (#t43 := MkT x)
    $ SR.rcons (#t44 := MkT x)
    $ SR.rcons (#t45 := MkT x)
    $ SR.rcons (#t46 := MkT x)
    $ SR.rcons (#t47 := MkT x)
    $ SR.rcons (#t48 := MkT x)
    $ SR.rcons (#t49 := MkT x)
      -- 50 .. 59
    $ SR.rcons (#t50 := MkT x)
    $ SR.rcons (#t51 := MkT x)
    $ SR.rcons (#t52 := MkT x)
    $ SR.rcons (#t53 := MkT x)
    $ SR.rcons (#t54 := MkT x)
    $ SR.rcons (#t55 := MkT x)
    $ SR.rcons (#t56 := MkT x)
    $ SR.rcons (#t57 := MkT x)
    $ SR.rcons (#t58 := MkT x)
    $ SR.rcons (#t59 := MkT x)
      -- 60 .. 69
    $ SR.rcons (#t60 := MkT x)
    $ SR.rcons (#t61 := MkT x)
    $ SR.rcons (#t62 := MkT x)
    $ SR.rcons (#t63 := MkT x)
    $ SR.rcons (#t64 := MkT x)
    $ SR.rcons (#t65 := MkT x)
    $ SR.rcons (#t66 := MkT x)
    $ SR.rcons (#t67 := MkT x)
    $ SR.rcons (#t68 := MkT x)
    $ SR.rcons (#t69 := MkT x)
      -- 70 .. 79
    $ SR.rcons (#t70 := MkT x)
    $ SR.rcons (#t71 := MkT x)
    $ SR.rcons (#t72 := MkT x)
    $ SR.rcons (#t73 := MkT x)
    $ SR.rcons (#t74 := MkT x)
    $ SR.rcons (#t75 := MkT x)
    $ SR.rcons (#t76 := MkT x)
    $ SR.rcons (#t77 := MkT x)
    $ SR.rcons (#t78 := MkT x)
    $ SR.rcons (#t79 := MkT x)
      -- 80 .. 89
    $ SR.rcons (#t80 := MkT x)
    $ SR.rcons (#t81 := MkT x)
    $ SR.rcons (#t82 := MkT x)
    $ SR.rcons (#t83 := MkT x)
    $ SR.rcons (#t84 := MkT x)
    $ SR.rcons (#t85 := MkT x)
    $ SR.rcons (#t86 := MkT x)
    $ SR.rcons (#t87 := MkT x)
    $ SR.rcons (#t88 := MkT x)
    $ SR.rcons (#t89 := MkT x)
      -- 90 .. 99
    $ SR.rcons (#t90 := MkT x)
    $ SR.rcons (#t91 := MkT x)
    $ SR.rcons (#t92 := MkT x)
    $ SR.rcons (#t93 := MkT x)
    $ SR.rcons (#t94 := MkT x)
    $ SR.rcons (#t95 := MkT x)
    $ SR.rcons (#t96 := MkT x)
    $ SR.rcons (#t97 := MkT x)
    $ SR.rcons (#t98 := MkT x)
    $ SR.rcons (#t99 := MkT x)
    $ SR.rnil
