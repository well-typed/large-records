#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Construct.Sized.R040 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row040 (Row)

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
    $ SR.rnil
