#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.SR_Construct.Sized.R020 where

import SuperRecord (Rec, (:=)(..))
import qualified SuperRecord as SR

import Bench.Types
import Common.RowOfSize.Row020 (Row)

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
    $ SR.rnil
