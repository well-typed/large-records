#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Unsafe.Sized.R070 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row070 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      SR.unsafeRCons (#t00 := MkT x)
    $ SR.unsafeRCons (#t01 := MkT x)
    $ SR.unsafeRCons (#t02 := MkT x)
    $ SR.unsafeRCons (#t03 := MkT x)
    $ SR.unsafeRCons (#t04 := MkT x)
    $ SR.unsafeRCons (#t05 := MkT x)
    $ SR.unsafeRCons (#t06 := MkT x)
    $ SR.unsafeRCons (#t07 := MkT x)
    $ SR.unsafeRCons (#t08 := MkT x)
    $ SR.unsafeRCons (#t09 := MkT x)
      -- 10 .. 19
    $ SR.unsafeRCons (#t10 := MkT x)
    $ SR.unsafeRCons (#t11 := MkT x)
    $ SR.unsafeRCons (#t12 := MkT x)
    $ SR.unsafeRCons (#t13 := MkT x)
    $ SR.unsafeRCons (#t14 := MkT x)
    $ SR.unsafeRCons (#t15 := MkT x)
    $ SR.unsafeRCons (#t16 := MkT x)
    $ SR.unsafeRCons (#t17 := MkT x)
    $ SR.unsafeRCons (#t18 := MkT x)
    $ SR.unsafeRCons (#t19 := MkT x)
      -- 20 .. 29
    $ SR.unsafeRCons (#t20 := MkT x)
    $ SR.unsafeRCons (#t21 := MkT x)
    $ SR.unsafeRCons (#t22 := MkT x)
    $ SR.unsafeRCons (#t23 := MkT x)
    $ SR.unsafeRCons (#t24 := MkT x)
    $ SR.unsafeRCons (#t25 := MkT x)
    $ SR.unsafeRCons (#t26 := MkT x)
    $ SR.unsafeRCons (#t27 := MkT x)
    $ SR.unsafeRCons (#t28 := MkT x)
    $ SR.unsafeRCons (#t29 := MkT x)
      -- 30 .. 39
    $ SR.unsafeRCons (#t30 := MkT x)
    $ SR.unsafeRCons (#t31 := MkT x)
    $ SR.unsafeRCons (#t32 := MkT x)
    $ SR.unsafeRCons (#t33 := MkT x)
    $ SR.unsafeRCons (#t34 := MkT x)
    $ SR.unsafeRCons (#t35 := MkT x)
    $ SR.unsafeRCons (#t36 := MkT x)
    $ SR.unsafeRCons (#t37 := MkT x)
    $ SR.unsafeRCons (#t38 := MkT x)
    $ SR.unsafeRCons (#t39 := MkT x)
      -- 40 .. 49
    $ SR.unsafeRCons (#t40 := MkT x)
    $ SR.unsafeRCons (#t41 := MkT x)
    $ SR.unsafeRCons (#t42 := MkT x)
    $ SR.unsafeRCons (#t43 := MkT x)
    $ SR.unsafeRCons (#t44 := MkT x)
    $ SR.unsafeRCons (#t45 := MkT x)
    $ SR.unsafeRCons (#t46 := MkT x)
    $ SR.unsafeRCons (#t47 := MkT x)
    $ SR.unsafeRCons (#t48 := MkT x)
    $ SR.unsafeRCons (#t49 := MkT x)
      -- 50 .. 59
    $ SR.unsafeRCons (#t50 := MkT x)
    $ SR.unsafeRCons (#t51 := MkT x)
    $ SR.unsafeRCons (#t52 := MkT x)
    $ SR.unsafeRCons (#t53 := MkT x)
    $ SR.unsafeRCons (#t54 := MkT x)
    $ SR.unsafeRCons (#t55 := MkT x)
    $ SR.unsafeRCons (#t56 := MkT x)
    $ SR.unsafeRCons (#t57 := MkT x)
    $ SR.unsafeRCons (#t58 := MkT x)
    $ SR.unsafeRCons (#t59 := MkT x)
      -- 60 .. 69
    $ SR.unsafeRCons (#t60 := MkT x)
    $ SR.unsafeRCons (#t61 := MkT x)
    $ SR.unsafeRCons (#t62 := MkT x)
    $ SR.unsafeRCons (#t63 := MkT x)
    $ SR.unsafeRCons (#t64 := MkT x)
    $ SR.unsafeRCons (#t65 := MkT x)
    $ SR.unsafeRCons (#t66 := MkT x)
    $ SR.unsafeRCons (#t67 := MkT x)
    $ SR.unsafeRCons (#t68 := MkT x)
    $ SR.unsafeRCons (#t69 := MkT x)
    $ SR.unsafeRNil 70
