#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-ds-preopt -ddump-ds -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiments.SR_Construct.Sized.R040 where

import SuperRecord

import Bench.Types
import Common.RowOfSize.Row040 (Row)

record :: Word -> Rec Row
record x =
      -- 00 .. 09
      rcons (#t00 := MkT x)
    $ rcons (#t01 := MkT x)
    $ rcons (#t02 := MkT x)
    $ rcons (#t03 := MkT x)
    $ rcons (#t04 := MkT x)
    $ rcons (#t05 := MkT x)
    $ rcons (#t06 := MkT x)
    $ rcons (#t07 := MkT x)
    $ rcons (#t08 := MkT x)
    $ rcons (#t09 := MkT x)
      -- 10 .. 19
    $ rcons (#t10 := MkT x)
    $ rcons (#t11 := MkT x)
    $ rcons (#t12 := MkT x)
    $ rcons (#t13 := MkT x)
    $ rcons (#t14 := MkT x)
    $ rcons (#t15 := MkT x)
    $ rcons (#t16 := MkT x)
    $ rcons (#t17 := MkT x)
    $ rcons (#t18 := MkT x)
    $ rcons (#t19 := MkT x)
      -- 20 .. 29
    $ rcons (#t20 := MkT x)
    $ rcons (#t21 := MkT x)
    $ rcons (#t22 := MkT x)
    $ rcons (#t23 := MkT x)
    $ rcons (#t24 := MkT x)
    $ rcons (#t25 := MkT x)
    $ rcons (#t26 := MkT x)
    $ rcons (#t27 := MkT x)
    $ rcons (#t28 := MkT x)
    $ rcons (#t29 := MkT x)
      -- 30 .. 39
    $ rcons (#t30 := MkT x)
    $ rcons (#t31 := MkT x)
    $ rcons (#t32 := MkT x)
    $ rcons (#t33 := MkT x)
    $ rcons (#t34 := MkT x)
    $ rcons (#t35 := MkT x)
    $ rcons (#t36 := MkT x)
    $ rcons (#t37 := MkT x)
    $ rcons (#t38 := MkT x)
    $ rcons (#t39 := MkT x)
    $ rnil
